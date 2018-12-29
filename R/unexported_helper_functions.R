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


# helper for river_voronoi
# moves starting and ending nodes either 0.0005 degrees, or 10 meters, depending on projection
move_nodes <- function(river, verbose=FALSE) {
    
    rivgeom <- st_geometry(river)
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




do_summary_fun <- function (list, funs, monthly,...) {
    output <- list()
    for(i in seq_along(list)) {
        data <- list[[i]]
        name <- names(list)[i]
        if(is.null(name)) name <- paste0("runoff",i)
        
        if(monthly) data$Month <- lubridate::month(data$Date)

        for (fun in funs) {
            
            # if input functions include quantile(), it needs to be handled in a special way so
            # that output list of tables have the correct names, and the table contains correct
            # column headers. If not for this, the output table has 2 or more times columns
            # leading to problems later on in downscaling.
            if (fun == "quantile" ) {
                if (!hasArg(probs)) stop('Argument probs for quantile missing')
                p <- list(...)

                p <- p[['probs']]

                for(prob in seq_along(p)) {
                    elname <- paste0(name,"_Q",p[prob]*100,"%")
                    if(monthly) {
                        out <- data %>% 
                            dplyr::select(-Date) %>%
                            dplyr::group_by(Month) %>% 
                            dplyr::summarise_all(.funs=fun, probs=p[prob])
                        output[[elname]] <- out
                    } else {
                        out <- data %>% 
                            dplyr::group_by(Date) %>% 
                            dplyr::summarise_all(.funs=fun, probs=p[prob])
                        output[[elname]] <- out
                    }
                }
                
            } else {
                elname <- paste0(name,"_",fun)
                if(monthly) {
                    out <- data %>% 
                        dplyr::select(-Date) %>%
                        dplyr::group_by(Month) %>% 
                        dplyr::summarise_all(.funs=fun)
                    output[[elname]] <- out
                } else {
                    out <- data %>% 
                        dplyr::group_by(Date) %>% 
                        dplyr::summarise_all(.funs=fun)
                    output[[elname]] <- out
                }
            }
            
            
        }
    }
    return(output)
}


summarise_over_all <- function (list) {
    n <- length(list)
    temp <- list[[1]]
    for(i in 2:n) {
        temp <- bind_rows(temp, list[[i]])
    }
    name <- paste0("runoff")
    out <- list(temp)
    names(out) <- name
    return(out)
}





# This is function ForecastComb::comb_CLS(), but edited according to 
# https://stackoverflow.com/a/28388394. 
# Edits marked with ###.
forecastcomb_comb_CLS <- function (x) 
{
    if (class(x) != "foreccomb") 
        stop("Data must be class 'foreccomb'. See ?foreccomb to bring data in 
             in a correct format.", 
             call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames
    p <- NCOL(prediction_matrix)
    Rinv <- solve(safe_chol(t(prediction_matrix) %*% prediction_matrix))
    C <- cbind(rep(1, p), diag(p))
    b = c(1, rep(0, p))
    d = t(observed_vector) %*% prediction_matrix
    nn2 = sqrt(norm(d,"2")) ###
    qp1 = solve.QP(Dmat = Rinv*nn2, factorized = TRUE, dvec = d/(nn2^2),  ###
                   Amat = C, bvec = b, meq = 1)
    weights = unname(qp1$sol)
    fitted <- as.vector(weights %*% t(prediction_matrix))
    accuracy_insample <- forecast::accuracy(fitted, observed_vector)
    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Constrained Least Squares Regression", 
                                 Models = modelnames, 
                                 Weights = weights, 
                                 Fitted = fitted, 
                                 Accuracy_Train = accuracy_insample, 
                                 Input_Data = list(Actual_Train = x$Actual_Train,
                                 Forecasts_Train = x$Forecasts_Train)), 
                            class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }
    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        pred <- as.vector(weights %*% t(newpred_matrix))
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Constrained Least Squares Regression", 
                                     Models = modelnames, 
                                     Weights = weights, 
                                     Fitted = fitted, 
                                     Accuracy_Train = accuracy_insample, 
                                     Forecasts_Test = pred, 
                                     Input_Data = list(Actual_Train = x$Actual_Train, 
                                                       Forecasts_Train = x$Forecasts_Train, 
                                                       Forecasts_Test = x$Forecasts_Test)), 
                                class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        }
        else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- forecast::accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Constrained Least Squares Regression", 
                                     Models = modelnames, 
                                     Weights = weights, 
                                     Fitted = fitted, 
                                     Accuracy_Train = accuracy_insample, 
                                     Forecasts_Test = pred, 
                                     Accuracy_Test = accuracy_outsample, 
                                     Input_Data = list(Actual_Train = x$Actual_Train,
                                                       Forecasts_Train = x$Forecasts_Train, 
                                                       Actual_Test = x$Actual_Test, 
                                                       Forecasts_Test = x$Forecasts_Test)),
                                class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}


# from package 'lme4'; needed in forecastcomb_comb_CLS()
safe_chol <- function(m) {
    if (all(m==0)) return(m)
    if (nrow(m)==1) return(sqrt(m))
    if (all(dmult(m,0)==0)) {  ## diagonal
        return(diag(sqrt(diag(m))))
    }
    ## attempt regular Chol. decomp
    if (!inherits(try(cc <- chol(m),silent=TRUE),"try-error"))
        return(cc)
    ## ... pivot if necessary ...
    cc <- suppressWarnings(chol(m,pivot=TRUE))
    oo <- order(attr(cc,"pivot"))
    cc[,oo]
    ## FIXME: pivot is here to deal with semidefinite cases,
    ## but results might be returned in a strange format: TEST
}

# from package 'lme4'; needed in safe_chol()
dmult <- function(m,s) {
    diag(m) <- diag(m)*s
    m
}





