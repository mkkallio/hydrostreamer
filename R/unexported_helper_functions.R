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
    
    out <- vector()
    
    if (dir[1] == 2) out <- c(out, adj[1])
    if (dir[2] == 4) out <- c(out, adj[2])
    if (dir[3] == 8) out <- c(out, adj[3])
    if (dir[4] == 1) out <- c(out, adj[4])
    if (dir[5] == 16) out <- c(out, adj[5])
    if (dir[6] == 128) out <- c(out, adj[6])
    if (dir[7] == 64) out <- c(out, adj[7])
    if (dir[8] == 32) out <- c(out, adj[8])
    
    if(length(out) == 1) return(NULL) else return(out)
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
    remove <- c("NEXT", "PREVIOUS", "DOWNSTREAM","zoneID")
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


# fixed: Vili Virkki 24-07-2019
new_row_col <- function (bearing, prc) {
  prep <- data.frame(row = 0, col = 0)
  if (bearing > -5 && bearing <= 22.5) {
    prep$row <- prc$row - 1
    prep$col <- prc$col
  } else if (bearing > 337.5 && bearing < 370) {
    prep$row <- prc$row - 1
    prep$col <- prc$col
  } else if (bearing > 22.5 && bearing <= 67.5) {
    prep$row <- prc$row - 1
    prep$col <- prc$col + 1
  } else if (bearing > 67.5 && bearing <= 112.5) {
    prep$row <- prc$row
    prep$col <- prc$col + 1
  } else if (bearing > 112.5 && bearing <= 157.5) {
    prep$row <- prc$row + 1
    prep$col <- prc$col + 1
  } else if (bearing > 157.5 && bearing <= 202.5) {
    prep$row <- prc$row + 1
    prep$col <- prc$col
  } else if (bearing > 202.5 && bearing <= 246.5) {
    prep$row <- prc$row + 1
    prep$col <- prc$col - 1
  } else if (bearing > 246.5 && bearing <= 292.5) {
    prep$row <- prc$row
    prep$col <- prc$col - 1
  } else if (bearing > 292.5 && bearing <= 337.5) {
    prep$row <- prc$row - 1
    prep$col <- prc$col - 1
  }
  return(prep)
}


collect_listc <- function(ts, acc = FALSE) {
    
    Date <- NULL
    
    unit <- units::deparse_unit(dplyr::pull(ts[[1]],2)) 
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
            act_ts[dates,i] <-unlist(ts[[i]][,tsi+1])
        }
        output[[ names[tsi] ]] <- units::as_units(act_ts, unit)     
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
    order <- c("riverID","zoneID", "NEXT", "PREVIOUS", "STRAHLER", "runoff_ts", 
               "discharge_ts", "observation_station", "observation_ts", 
               "control_type", "control_ts")
    order <- order[order %in% names(HS)]
    
    HS <- dplyr::select(HS, order, dplyr::everything())
    return(HS)
}


# if object has "HS" attributes, edit those without NULL
# if object doesnt have "HS" attributes, initialize it with provided values
mod_HS_attributes <- function(HS, next_col = NA, prev_col = NA, 
                              col = NULL) {
    
    if(is.null(col)) {
        test <- is.null(base::attr(HS, "HS", exact = TRUE))
        if(test) {
            att <- c(next_col = next_col, 
                     prev_col = prev_col)
            base::attr(HS, "HS") <- att
        } else {
            att <- base::attr(HS, "HS", exact = TRUE)
            
            if(!is.na(next_col)) att["next_col"] <- next_col
            if(!is.na(prev_col)) att["prev_col"] <- prev_col
            
            base::attr(HS, "HS") <- att
        }
        return(HS)
    } else {
        test <- hasName(HS, col)
        if(!test) stop("Couldn't find column ", col, " in HS input.")
        
        ind <- which(colnames(HS) == col)
        test <- is.null(base::attr(HS[[ind]], "HS", exact = TRUE))
        if(test) {
            att <- c(next_col = next_col, 
                     prev_col = prev_col)
            base::attr(HS[[ind]], "HS") <- att
        } else {
            att <- base::attr(HS[[ind]], "HS", exact = TRUE)
            
            if(!is.na(next_col)) att["next_col"] <- next_col
            if(!is.na(prev_col)) att["prev_col"] <- prev_col
            
            base::attr(HS[[ind]], "HS") <- att
        }
        return(HS)
    }
    
    
}
# 
get_HS_attr <- function(HS) {
    return(base::attr(HS, "HS", exact = TRUE))
}

# goes through the columns in HS, and checks the value of "HS"
find_attribute <- function(HS, attribute, value) {
    test <- sapply(HS, function(x) {
        att <- attr(x, "HS") 
        test <- att[attribute] == value
        if(length(test) == 0) return(FALSE) else return(test)
    })
    return(which(test))
}

# convert unit 
convert_unit <- function(value, from = NULL, to1, to2, verbose = FALSE) {
    
    test <- inherits(value, "units")
    if(!test) {
        test <- is.null(from)
        if(test) stop("input is not of class 'units', and 'from' not specified")
        value <- units::set_units(value, from, mode="standard")
    }
    
    
    #try converting to first unit
    conv <- try(units::set_units(value, to1, mode="standard"),
                silent = TRUE)
    
    # if first conversion fails, try converting to alternative
    if(class(conv) == "try-error") {
        conv <- try(units::set_units(value, to2, mode="standard"),
                    silent = TRUE)
        
        # stop conversion to alternative didnt work
        if(class(conv) == "try-error") {
            stop(paste0("Couldn't convert units. Are you sure the unit's type is ",
                        "depth per time (e.g. mm/d) or volume per time ",
                        "(e.g. m^3/s)?"))
        }
        
        # return alternative
        if(verbose) message("Unit converted from ", from, " to ", to2)
        return(conv)
    }
    
    # return first option
    if(verbose) message("Unit converted from ", from, " to ", to1)
    return(conv)
}

unit_conversion <- function(obj, unit, areas = NULL) {
    units <- strsplit(unit, " ")[[1]]
    
    # check that all required elements are there and convertible
    depth <- any(units %in% c("mm", "cm", "m", "km"))
    area <- any(units %in% c("m-2", "km-2", "ha"))
    time <- any(units %in% c("s-1", "min-1", "h-1", "d-1", 
                             "week-1", "month-1"))
    volume <- any(units %in% c("m3", "km3"))
    
    if(volume & time) {
        obj <- convert_unit(obj, to1 = "m3/s")
    } else if(volume & time & area) {
        obj <- units::set_units(obj, "m3 m-2 s-1")
        areas <- units::set_units(areas, "m2")
        obj <- obj * areas
    } else if(depth & time & !area) {
        obj <- units::set_units(obj, "m s-1")
        areas <- units::set_units(areas, "m2")
        obj <- obj * areas
    } else if(depth & time & area) {
        obj <- units::set_units(obj, "m m-2 s-1")
        areas <- units::set_units(areas, "m2")
        obj <- obj * areas
        obj <- obj * as_units(1, "m2")
    } 
    return(obj)
}
