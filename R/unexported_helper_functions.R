#helper function to compute weights for line segments
compute_segment_weights <- function(segments, variable) {
    weights <- rep(0, length(segments))
    n <- sum(variable[segments])
    weights[segments] <- variable[segments]/n
    return(weights)
}

compute_dasymetric_weights <- function(segments, arealen, variable) {
    weights <- rep(0, length(variable))
    arealenvar <- arealen*variable
    denom <- sum(arealen[segments]*variable[segments])
    weights[segments] <- arealenvar[segments]/denom
    return(weights)
} 



#helper function for delineate_basin
next_cell_up <- function(cell, drain.dir) {
    
    adj <- raster::adjacent(drain.dir, cell, 
                            directions=8, 
                            sorted=TRUE, 
                            pairs=FALSE)
    if (length(adj) < 8) return(NULL)
    dir <- drain.dir[adj]
    
    out <- 0
    
    if (dir[1] == 2) out <- c(out, adj[1])
    if (dir[2] == 4) out <- c(out, adj[2])
    if (dir[3] == 8) out <- c(out, adj[3])
    if (dir[4] == 1) out <- c(out, adj[4])
    if (dir[5] == 16) out <- c(out, adj[5])
    if (dir[6] == 128) out <- c(out, adj[6])
    if (dir[7] == 64) out <- c(out, adj[7])
    if (dir[8] == 32) out <- c(out, adj[8])
    
    if(length(out) == 1) return(NULL) else return(out[2:length(out)])
}


# helper for river_voronoi
# moves starting and ending nodes either 0.0005 degrees, or 10 meters, 
# depending on projection
move_nodes <- function(river, verbose=FALSE) {
    
    rivgeom <- sf::st_geometry(river)
    p4s <- sf::st_crs(river)[2]
    
    n <- nrow(river)
    if (verbose) pb <- txtProgressBar(min = 0, max = n, style = 3)
    
    if( grepl("longlat", p4s, fixed=TRUE) ) {
        dist <- 0.0005
        for (line in 1:nrow(river)) {
            coords <- sf::st_coordinates(river[line,])
            len <- NROW(coords)
            firstpair <- coords[2:1,1:2]
            lastpair <- coords[(len-1):len,1:2]
            
            firstbear <- geosphere::bearing(firstpair)[1] +180
            lastbear <- geosphere::bearing(lastpair)[1] +180
            
            move <- move_coords(firstbear,dist)
            coords[1,1:2] <- coords[1,1:2] + move
            
            move <- move_coords(lastbear,dist)
            coords[len,1:2] <- coords[len,1:2] + move
            
            sfc <- sf::st_linestring(coords[,1:2], dim="XY") %>% 
                sf::st_sfc()
            rivgeom[line] <- sfc
            
            if (verbose) setTxtProgressBar(pb, line)
        }
        rivgeom <- sf::st_set_geometry(river, rivgeom)
    } else {
        dist <- 10
        for (line in 1:n) {
            coords <- sf::st_coordinates(river[line,])
            len <- NROW(coords)
            firstpair <- coords[2:1,1:2]
            lastpair <- coords[(len-1):len,1:2]
            
            
            dx <- firstpair[1,1]-firstpair[2,1]
            dy <- firstpair[1,2]-firstpair[2,2]
            angle <- 90 - (180/pi)*atan2(dy,dx)
            angle <- angle %% 360
            move <- move_coords(angle,dist)
            coords[1,1:2] <- coords[1,1:2] + move
            
            dx <- lastpair[1,1]-lastpair[2,1]
            dy <- lastpair[1,2]-lastpair[2,2]
            angle <- 90 - (180/pi)*atan2(dy,dx)
            angle <- angle %% 360
            move <- move_coords(angle,dist)
            coords[len,1:2] <- coords[len,1:2] + move
            
            sfc <- sf::st_linestring(coords[,1:2], dim="XY") %>% 
                sf::st_sfc() 
            rivgeom[line] <- sfc
            
            if (verbose) setTxtProgressBar(pb, line)
        }
        rivgeom <- sf::st_set_geometry(river, rivgeom)
        
    }
    if (verbose) close(pb)
    return(rivgeom)
}


move_coords <- function(bear, dist) {
    movex <- dist * sin(bear*pi/180)
    movey <- dist * cos(bear*pi/180)
    return(c(x=movex, y=movey))
}

tesselate_voronoi <- function(vorPoints, 
                              aoi, 
                              riverID = "riverID", 
                              verbose = FALSE) {
    if (verbose) message("Processing Voronoi tesselation")
    vorPoints <- suppressWarnings(sf::st_cast(vorPoints, "POINT"))
    remove <- c("NEXT", "PREVIOUS", "DOWNSTREAM","gridID")
    voronoi <- vorPoints[ , !(names(vorPoints) %in% remove)] %>%
        dplyr::rename_(ID = riverID)
    bbox <- sf::st_as_sfc(sf::st_bbox(aoi))
    voronoi <- suppressMessages(
                suppressWarnings(
                    sf::st_voronoi(sf::st_union(vorPoints), bbox) %>%
                         sf::st_cast() %>%
                         sf::st_cast("POLYGON") %>%
                         sf::st_sf() %>%
                         sf::st_join(vorPoints) %>%
                         lwgeom::st_make_valid() %>% 
                         dplyr::group_by_(.dots = list(~ID)) %>%
                         dplyr::summarise() %>%
                         sf::st_intersection(sf::st_geometry(aoi))))
    return(voronoi)
}


fix_voronoi <- function(voronoi, riverID = "riverID", verbose = FALSE) {
    #------
    # the process may generate geometrycollections instead of polygons --> 
    # this will correct them
    v.gc <- sf::st_is(voronoi, "GEOMETRYCOLLECTION")
    
    if (any(v.gc)) {
        if (verbose) message("Fixing bad polygons (GEOMETRYCOLLECTION)")
        p.geom <- voronoi[v.gc,]
        p.geom <- sf::st_collection_extract(p.geom, "POLYGON")
        voronoi[v.gc,] <- p.geom
    }
    #------
    
    
    #------
    # sometimes there are voronoi areas left which were not assigned any ID, 
    # and thus not dissolved. The following code merges them to the 
    # neighbouring polygon with which it shares the longest border segment.
    
    
    IDs <- voronoi[, names(voronoi) %in% riverID] %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    v.na <- is.na(IDs)
    
    if (any(v.na)) {
        if (verbose) message("Fixing bad polygons (missing ID)")
        pp <- sf::st_cast(voronoi[v.na,], "POLYGON")
        for (i in 1:NROW(pp)) {
            #find which polygons touch the problem polygon
            touching <- suppressMessages(
                            sf::st_touches(pp[i,], voronoi, sparse=FALSE))
            tv <- voronoi[touching,]
            
            # compute the lenghts of border line for every touching polygon 
            # and find which one is longest
            len <- list()
            n <- table(touching)[2]
            for (tp in 1:n) {
                line <- suppressMessages(sf::st_intersection(pp[i,], tv[tp,]))
                l_len <- sf::st_length(line)
                len[[tp]] <- l_len
            }
            len <- unlist(len)
            longest <- which(len == max(len))
            
            #union the two polygons
            v.union <- suppressMessages(
                        suppressWarnings(
                            sf::st_union(pp[i,], tv[longest,])))
            
            #replace geometry in voronoi
            row <- which(IDs == IDs[touching][longest])
            v.union <- sf::st_set_geometry(voronoi[row,], 
                                           sf::st_geometry(v.union))
            voronoi[row,] <- v.union
        }
        #remove geometries with NA id
        voronoi <- voronoi[!v.na,]
        
    }
    #------
    return(voronoi)
}



new_row_col <- function(bearing, prc) {
    prep <- data.frame(row=0, col=0)
    if (bearing > -5 && bearing < 5) {
        prep$row <- prc$row-1
        prep$col <- prc$col
    } else if (bearing > 350 && bearing < 370) {
        prep$row <- prc$row-1
        prep$col <- prc$col
    } else  if (bearing > 40 && bearing < 50) {
        prep$row <- prc$row-1
        prep$col <- prc$col+1
    } else if (bearing > 80 && bearing < 100) {
        prep$row <- prc$row
        prep$col <- prc$col+1
    } else if (bearing > 125 && bearing < 145) {
        prep$row <- prc$row+1
        prep$col <- prc$col+1
    } else if (bearing > 170 && bearing < 190) {
        prep$row <- prc$row+1
        prep$col <- prc$col
    } else if (bearing > 215 && bearing < 235) {
        prep$row <- prc$row+1
        prep$col <- prc$col-1
    } else if (bearing > 260 && bearing < 280) {
        prep$row <- prc$row
        prep$col <- prc$col-1
    } else if (bearing > 305 && bearing < 325) {
        prep$row <- prc$row-1
        prep$col <- prc$col-1
    }
    return(prep)
} 



collect_listc <- function(ts, acc = FALSE) {
    
    Date <- NULL
    
    unidates <- lapply(ts, function(x) x$Date) %>%
        unlist %>%
        unique %>%
        lubridate::as_date()
    names <- colnames(ts[[1]])[-1]
    nts <- length(ts)
    #n <- ncol(ts[[1]])-1
    n <- lapply(ts, ncol) %>%
        unlist %>%
        max
    n <- n-1
    empty_ts <- matrix(NA, ncol = nts, nrow = length(unidates))
    
    output <- list()
    
    for (tsi in 1:n) {
        act_ts <- empty_ts
        for(i in 1:nts) {
            if(ncol(ts[[i]])-1 < tsi) break
            dates <- unidates %in% ts[[i]]$Date 
            act_ts[dates,i] <- unlist(ts[[i]][,tsi+1])
        }
        output[[ names[tsi] ]] <- act_ts     
    }
    
    if(acc) {
        for(tsi in seq_along(output)) {
            temp <- cbind(unidates, output[[tsi]]) %>%
                as.data.frame()
            colnames(temp) <- c("Date", names(ts))
            temp <- temp %>% 
                dplyr::mutate(Date = lubridate::as_date(Date)) %>%
                tibble::as_tibble() %>%
                tsibble::as_tsibble(index = "Date")
            output[[tsi]] <- temp
        }
    }
    
    names(output) <- names
    
    return(output)
}


init_ts <- function(ts) {
    max_ts <- lapply(ts, nrow) %>%
        unlist
    max_ts <- which(max_ts == max(max_ts))
    dates <- ts[[ max_ts[1] ]]$Date
    
    output <- vector("list", ncol(ts[[1]])-1) 
    
    output <- lapply(output, function(x) {
        tsib <- matrix(NA, ncol=length(ts)+1, nrow=length(dates)) 
        colnames(tsib) <- c("Date", names(ts))
        tsib <- tibble::as_tibble(tsib)
        tsib$Date <- dates
        tsib <- tsibble::as_tsibble(tsib, index = "Date")
        return(tsib)
    })
    names(output) <- colnames(ts[[1]])[-1]
    return(output)
}

spread_listc <- function(ts) {
    listc <- list()
    names <- names(ts)
    
    listc <- init_ts(ts)
    
    for(seg in seq_along(listc)) {
        for(tsi in seq_along(ts)) {
            listc[[seg]][,tsi+1] <- ts[[tsi]][seg+1]
        }
    }
    return(listc)
}


assign_class <- function(obj, class) {
    
    original <- class(obj)
    
    HS <- which(original %in% class)
    
    if(HS != 1 || length(HS) == 0) {
        
        if (length(HS) != 0) newclass <- c(class, original[-HS])
        if (length(HS) == 0) newclass <- c(class, original)
        class(obj) <- newclass
        return(obj)
        
    } else {
        
        return(obj)
        
    }
}

reorder_cols <- function(HS) {
    order <- c("riverID","gridID", "NEXT", "PREVIOUS", "STRAHLER", "runoff_ts", 
               "discharge_ts", "observation_station", "observation_ts", 
               "control_type", "control_ts")
    order <- order[order %in% names(HS)]
    
    HS <- dplyr::select(HS, order, dplyr::everything())
    return(HS)
}



