#' Computes specific runoff generated in the river segment.
#' 
#' Computes downscaled, river segment specific, runoff using 
#' the given \code{HSweights} object. 
#'  
#' @param HSweights An object of class 'HSweights', obtained with 
#'   \code{compute_HSweights}, or constructed with function
#'   \code{create_HSweights}.
#' @param unit Unit of runoff. Can be either "mm/s", or "m3/s". 
#'   Defaults to mm/s (~ equivalent to kg/m2/s).
#' @param pycno If \code{TRUE}, ignore weights and perform pycnophylactic
#'   interpolation for every timestep. EXPERIMENTAL AND 2-3 ORDERS OF MAGNITUDE
#'   SLOWER THAN USING WEIGHTS. Default FALSE.
#' @param n Iterations in the pycnophylactic interpolation.
#' @param dasy name of the dasymetric variable in \code{HSweights$weights}.
#'   Experimental, used in pycnophylactic interpolation.
#' @param verbose Print progress indication or not.
#'
#' @return The routed river network object with class \code{HS}, which has been 
#'   enhanced with a runoff timeseries (list column \code{runoff_ts}. Runoff is 
#'   given in \eqn{m^3/s}.
#' 
#' @export
downscale_runoff <- function(HSweights, 
                             unit = "mm/s",
                             pycno = FALSE,
                             n = 10,
                             dasy = NULL,
                             verbose = FALSE) {
    
    if(!any("HSweights" %in% class(HSweights))) {
        stop("Input should be of class HSweights.")
    }
    
    ##############
    # DOWNSCALE
    if(pycno) {
        if(!is.null(dasy)) {
            if(!hasName(HSweights$weights, dasy)) {
                stop(paste0("Column name ", dasy, 
                            " not found in HSweights$weights"))
            }
            dasy <- pull(HSweights$weights, dasy)
        }
        QTS <- downscale_pycno(HSweights,
                               n, 
                               dasy = dasy,
                               unit = unit, 
                               verbose = verbose)
    } else {
        QTS <- downscale_with_weights(HSweights, 
                                      unit = unit,
                                      verbose = verbose)
    }
    
    listc <- spread_listc(QTS)
    listc <- listc[order(names(listc))]
    
    output <- HSweights$river %>% 
        dplyr::arrange(riverID)
    output <- tibble::add_column(output, runoff_ts = listc) %>%
        tibble::as_tibble() %>%
        sf::st_as_sf()
    
    output <- reorder_cols(output)
    output <- assign_class(output, "HS")
    return(output)

} 


######## DOWNSCALING BASED ON WEIGHTS
downscale_with_weights <- function(HSweights, 
                                   unit = "mm/s",
                                   verbose = FALSE) {
    #####################
    # prepare
    
    area_m2 <- NULL
    gridID <- NULL
    Date <- NULL
    riverID <- NULL
    
    river <- HSweights$river
    weights <- HSweights$weights
    grid <- HSweights$grid
    
    nriv <- NROW(river)
    nseg <- NROW(weights)
    ng <- NROW(grid)
    
    rIDs <- dplyr::select_(river, "riverID") %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    gIDs <- dplyr::select(grid, gridID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    wrIDs <- dplyr::select_(weights, "riverID") %>% 
        sf::st_set_geometry(NULL) %>%
        unlist() %>%
        match(rIDs)
    wgIDs <- dplyr::select(weights, gridID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist() %>%
        match(gIDs)
    weightvec <- dplyr::select(weights, weights) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    gridareas <- dplyr::select(grid, area_m2) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    
    if (unit == "mm/s") convert <- TRUE
    if (unit == "m3/s") convert <- FALSE
    
    runoff_ts <- collect_listc(grid$runoff_ts)
    ngrids <- length(runoff_ts)
    unidates <- lapply(grid$runoff_ts, function(x) x$Date) %>%
        unlist %>%
        unique %>%
        lubridate::as_date()
    
    total <- ngrids
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    qts <- list()
    for (g in 1:ngrids) {
        
        names <- colnames(runoff_ts[[g]])[-1]
        nts <- nrow(runoff_ts[[g]])
        runoffTS <- runoff_ts[[g]]
        
        QTS <- matrix(0, nrow = nts, ncol = nriv)
        
        if(convert) {
            for (seg in 1:nseg) {
                QTS[, wrIDs[seg] ] <- QTS[, wrIDs[seg] ] + 
                    weightvec[seg] *
                    runoffTS[, wgIDs[seg] ] *
                    gridareas[ wgIDs[seg] ] / 1000
                
            }
        } else {
            for (seg in 1:nseg) {
                QTS[, wrIDs[seg] ] <- QTS[, wrIDs[seg] ] + 
                    weightvec[seg] *
                    runoffTS[, wgIDs[seg] ]
            }
        }
        
        
        QTS <- data.frame(QTS)
        colnames(QTS) <- rIDs
        QTS$Date <- unidates
        QTS <- dplyr::select(QTS, Date, dplyr::everything()) 
        
        
        if(is.null(colnames(grid$runoff_ts[[1]]))) {
            qts[[g]] <- QTS
        } else {
            name <- colnames(grid$runoff_ts[[1]])[g+1]
            qts[[ name ]] <- QTS
        }
        
        if (verbose) setTxtProgressBar(pb, g)
    }
    if (verbose) close(pb)
    
    return(qts)
    
}

##### PYCNOPHYLACTIC INTERPOLATION FOR POLYGON NETWORKS
downscale_pycno <- function(HS, n, dasy = NULL, 
                            unit = "mm/s", verbose = FALSE) {
    
    if(verbose) message("Preprocessing..")
    
    if (unit == "mm/s") convert <- TRUE
    if (unit == "m3/s") convert <- FALSE
    
    ##################
    # create padding
    outer_buffer <- sf::st_union(HS$weights) %>%
        # st_cast("POLYGON") %>%
        sf::st_cast("LINESTRING") %>%
        sf::st_buffer(dist = 0.01)
    
    # break padding by centroids of neighbours, join attributes
    outer_basin_centroids <- sf::st_intersection(HS$weights, outer_buffer) %>%
        sf::st_centroid() 
    
    outer_basin <- outer_basin_centroids %>%
        sf::st_union() %>%
        sf::st_voronoi() %>%
        sf::st_cast() %>%
        sf::st_sf() %>%
        sf::st_join(outer_basin_centroids) %>%
        sf::st_intersection(outer_buffer) %>%
        sf::st_difference(st_union(HS$weights)) %>%
        dplyr::mutate(outID = 1:nrow(.), 
               orig_id = riverID,
               riverID = NA) %>%
        dplyr::select(outID, orig_id, riverID, gridID, dplyr::everything())
    
    ####
    # prepare for pp
    pycno <- rbind(outer_basin, 
                   HS$weights %>% 
                       tibble::add_column(outID = NA, orig_id = NA) %>%
                       dplyr::select(outID, orig_id, riverID, 
                                     dplyr::everything()) %>%
                       dplyr::rename(geometry = geom))
    
    #identify neighbours
    touching <- sf::st_touches(pycno)
    
    
    #prep
    nmod <- ncol(HS$grid$runoff_ts[[1]])
    nstep <- nrow(HS$grid$runoff_ts[[1]])
    names <- colnames(HS$grid$runoff_ts[[1]])
    nriver <- nrow(HS$river)
    rivers <- which(!is.na(pycno$riverID))
    gridareas <- HS$grid$area_m2
    unidates <- lapply(HS$grid$runoff_ts, function(x) x$Date) %>%
        unlist %>%
        unique %>%
        lubridate::as_date()
    
    QTS <- list() 
    
    total <- nmod-1
    if(verbose) message("Performing pycnophylactic downscaling..")
    if(verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    # do for each timestep and each model, convert mm/s to m3/s?
    for(model in 2:nmod) {
        qts <- matrix(NA, nrow = nstep, ncol = nriver)
        colnames(qts) <- HS$river$riverID
        name <- names[model]

        for(tstep in 1:nstep) {
            
            r_grid <- sapply(HS$grid$runoff_ts, 
                             function(x) pull(x[tstep, model]))
            
            r_pycno <- iterate_pycno(pycno, dasy, touching, 
                                     r_grid, gridareas, n)
            if(convert) r_pycno <- r_pycno * pycno$b_area_m2 / 1000
            
            for(i in seq_along(r_pycno)) {
                where <- which(HS$river$riverID == pycno$riverID[i])
                if(length(where) == 0) next
                qts[tstep, where] <- sum(qts[tstep, where], 
                                        r_pycno[i],
                                        na.rm=TRUE) 
            }

        }
      
        qts <- data.frame(qts)
        colnames(qts) <- HS$river$riverID
        qts <- tibble::add_column(qts, Date = unidates, .before=1)
        QTS[[ name ]] <- qts
        
        if (verbose) setTxtProgressBar(pb, model)
    }
    close(pb)
    if (verbose) message("Processing output..")
    
    return(QTS)
    
}


iterate_pycno <- function(pycno, dasy = NULL, touching, r_grid, gridareas, n) {
    
    # prepare
    r_curr <- rep(NA, length(touching))
    for(i in seq_along(r_grid)) {
        ind <- which(pycno$gridID == as.numeric(names(r_grid)[i]))
        r_curr[ind] <- r_grid[[i]]
    }
    r_prev <- r_curr
    
    iter <- which(!is.na(pycno$riverID))
    remove <- which(is.na(pycno$riverID))
    pycno$gridID[remove] <- NA
    
    # iterate
    for (i in 1:n) {

        for(j in iter) {
            ind <- c(j, touching[[j]])
            new_value <- mean(r_prev[ind], na.rm=TRUE)
            r_curr[j] <- new_value
        }
        
        # rescale
        if(convert) {
            for(j in seq_along(r_grid)) {
                ind <- which(pycno$gridID == j)
                sum_runoff <- sum(r_curr[ind] * pycno$b_area_m2[ind], na.rm = TRUE)
                bias <- r_grid[j]*gridareas[j] / sum_runoff
                r_curr[ind] <- r_curr[ind] * bias
            }
        } else {
            for(j in seq_along(r_grid)) {
                ind <- which(pycno$gridID == j)
                mean_runoff <- mean(r_curr[ind])
                bias <- r_grid[j] / mean_runoff
                r_curr[ind] <- r_curr[ind] * bias
            }
        }
        
        r_prev <- r_curr    
    }
    
    # if dasymetric variable is provided
    if(!is.null(dasy)) {
        r_curr <- r_curr * dasy
        
        #rescale again
        if(convert) {
            for(j in seq_along(r_grid)) {
                ind <- which(pycno$gridID == j)
                sum_runoff <- sum(r_curr[ind] * pycno$b_area_m2[ind], na.rm = TRUE)
                bias <- r_grid[j]*gridareas[j] / sum_runoff
                r_curr[ind] <- r_curr[ind] * bias
            }
        } else {
            for(j in seq_along(r_grid)) {
                ind <- which(pycno$gridID == j)
                mean_runoff <- mean(r_curr[ind])
                bias <- r_grid[j] / mean_runoff
                r_curr[ind] <- r_curr[ind] * bias
            }
        }
    }
    
    return(r_curr)
} 


