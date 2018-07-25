#helper function to compute weights for line segments
compute_segment_weights <- function(segments, variable) {
    weights <- rep(0, length(segments))
    n <- sum(variable[segments])
    weights[segments] <- variable[segments]/n
    return(weights)
}



#helper function for delineate_basin
next_cell_up <- function(cell, drain.dir) {
    
    adj <- raster::adjacent(drain.dir, cell, directions=8, sorted=TRUE, pairs=FALSE)
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



move_nodes <- function(river, p4s, verbose=FALSE) {
    vorRiv <- st_geometry(river)
    n <- nrow(river)
    if (verbose) pb <- txtProgressBar(min = 0, max = n, style = 3)
    if( grepl("longlat", p4s, fixed=TRUE) ) {
        dist <- 0.0005
        for (line in 1:nrow(river)) {
            coords <- sf::st_coordinates(river[line,])
            len <- NROW(coords)
            
            bear <- geosphere::bearing(coords[(len-1):len,1:2])[1] 
            if(bear > 90) bear <- abs(bear)-90 else
                if(bear < -90) bear <- -(bear+270) else
                    if(bear < 90 && bear >= 0) bear <- 90-bear else
                        if(bear < 0 && bear >= -90) bear <- -bear+90
            
            newx <- coords[len,1] - dist * cos(bear*pi/180)
            newy <- coords[len,2] - dist * sin(bear*pi/180)
            coords[len,1:2] <- c(newx,newy)
            
            
            bear <- geosphere::bearing(coords[1:2,1:2])[1]
            if(bear > 90) bear <- abs(bear)-90 else
                if(bear < -90) bear <- -(bear+270) else
                    if(bear < 90 && bear >= 0) bear <- 90-bear else
                        if(bear < 0 && bear >= -90) bear <- -bear+90
            
            newx <- coords[1,1] + dist * cos(bear*pi/180)
            newy <- coords[1,2] + dist * sin(bear*pi/180)
            coords[1,1:2] <- c(newx,newy)
            
            sfc <- sf::st_linestring(coords[,1:2], dim="XY") %>% 
                sf::st_sfc() #%>% 
            #st_set_crs(p4s) %>% 
            #sf::st_sf() %>% 
            #cbind(ID = ID[line])
            vorRiv[line] <- sfc
            
            if (verbose) setTxtProgressBar(pb, line)
        }
        vorRiv <- st_set_geometry(river, vorRiv)
    } else {
        dist <- 10
        for (line in 1:n) {
            coords <- sf::st_coordinates(river[line,])
            len <- NROW(coords)
            
            a <- coords[len-1,1:2]
            b <- coords[len,1:2]
            #theta <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
            theta = atan2(a[2] - b[2], a[1] - b[1])
            newx <- coords[len,1] + dist * cos(theta)
            newy <- coords[len,2] + dist * sin(theta)
            coords[len,1:2] <- c(newx,newy)
            
            
            a <- coords[1,1:2]
            b <- coords[2,1:2]
            #theta <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
            theta = atan2(a[2] - b[2], a[1] - b[1])
            newx <- coords[1,1] - dist * cos(theta)
            newy <- coords[1,2] - dist * sin(theta)
            coords[1,1:2] <- c(newx,newy)
            
            sfc <- sf::st_linestring(coords[,1:2], dim="XY") %>% 
                sf::st_sfc() #%>% 
            #st_set_crs(p4s) %>% 
            #sf::st_sf() %>% 
            #cbind(ID = ID[line])
            vorRiv[line] <- sfc
            
            if (verbose) setTxtProgressBar(pb, line)
        }
        vorRiv <- st_set_geometry(river, vorRiv)
        
    }
    if (verbose) close(pb)
    return(vorRiv)
}

tesselate_voronoi <- function(vorPoints, aoi, riverID = "riverID", verbose = FALSE) {
    if (verbose) message("Processing Voronoi tesselation")
    vorPoints <- suppressWarnings(sf::st_cast(vorPoints, "POINT"))
    remove <- c("NEXT", "PREVIOUS", "DOWNSTREAM","gridID")
    voronoi <- vorPoints[ , !(names(vorPoints) %in% remove)] %>%
        dplyr::rename_(ID = riverID)
    bbox <- sf::st_as_sfc(sf::st_bbox(aoi))
    voronoi <- suppressMessages(suppressWarnings(sf::st_voronoi(sf::st_union(vorPoints), bbox) %>%
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
    # the process may generate geometrycollections instead of polygons --> this will correct them
    v.gc <- sf::st_is(voronoi, "GEOMETRYCOLLECTION")
    
    if (any(v.gc)) {
        if (verbose) message("Fixing bad polygons (GEOMETRYCOLLECTION)")
        p.geom <- voronoi[v.gc,]
        p.geom <- sf::st_collection_extract(p.geom, "POLYGON")
        voronoi[v.gc,] <- p.geom
    }
    #------
    
    
    #------
    # sometimes there are voronoi areas left which were not assigned any ID, and thus not dissolved. 
    # The following code merges them to the neighbouring polygon with which it shares the longest 
    # border segment.
    
    
    IDs <- voronoi[, names(voronoi) %in% riverID] %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    v.na <- is.na(IDs)
    
    if (any(v.na)) {
        if (verbose) message("Fixing bad polygons (missing ID)")
        pp <- sf::st_cast(voronoi[v.na,], "POLYGON")
        for (i in 1:NROW(pp)) {
            #find which polygons touch the problem polygon
            touching <- suppressMessages(sf::st_touches(pp[i,], voronoi, sparse=FALSE))
            tv <- voronoi[touching,]
            
            #compute the lenghts of border line for every touching polygon and find which one is longest
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
            v.union <- suppressMessages(suppressWarnings(sf::st_union(pp[i,], tv[longest,])))
            
            #replace geometry in voronoi
            row <- which(IDs == IDs[touching][longest])
            v.union <- sf::st_set_geometry(voronoi[row,], sf::st_geometry(v.union))
            voronoi[row,] <- v.union
        }
        #remove geometries with NA id
        voronoi <- voronoi[!v.na,]
        
    }
    #------
    return(voronoi)
}