#' Computes specific runoff generated in the river segment.
#' 
#' Computes downscaled, river segment specific, runoff using 
#' the given \code{HSweights} object. 
#'  
#' @param HSweights An object of class 'HSweights', obtained with 
#'   \code{compute_HSweights}, or constructed with function
#'   \code{create_HSweights}.
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
downscale_runoff <- function(HSweights,
                             pycno = FALSE,
                             n = 10,
                             dasy = NULL,
                             verbose = FALSE) {
    
    riverID <- NULL
    
    if(!inherits(HSweights, "HSweights")) {
        stop("Input should be of class HSweights. See ?compute_HSweights()")
    }
    
    ##############
    # DOWNSCALE
    if(pycno) {
        if(!is.null(dasy)) {
            if(!hasName(HSweights$weights, dasy)) {
                stop(paste0("Column name ", dasy, 
                            " not found in HSweights$weights"))
            }
            dasy <- dplyr::pull(HSweights$weights, dasy)
        }
        QTS <- downscale_pycno(HSweights,
                               n, 
                               dasy = dasy,
                               verbose = verbose)
    } else {
        QTS <- downscale_with_weights(HSweights, 
                                      verbose = verbose)
    }
    
    listc <- spread_listc(QTS)
    listc <- listc[order(as.numeric(names(listc)))]
    
    output <- HSweights$target %>% 
        dplyr::arrange(riverID) %>% 
        tibble::add_column(runoff_ts = listc) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        sf::st_as_sf()
    
    output <- reorder_cols(output)
    output <- assign_class(output, "HS")
    return(output)
    
} 


######## DOWNSCALING BASED ON WEIGHTS
downscale_with_weights <- function(HSweights,
                                   verbose = FALSE) {
    #---------------------------------------------------------------------------
    # prepare
    
    area <- NULL
    zoneID <- NULL
    Date <- NULL
    riverID <- NULL
    
    river <- HSweights$target
    weights <- units::drop_units(HSweights$weights)
    grid <- HSweights$source
    
    nriv <- NROW(river)
    nseg <- NROW(weights)
    ng <- NROW(grid)

    rIDs <- dplyr::pull(river, riverID)
    gIDs <- dplyr::pull(grid, zoneID)
    wrIDs <- dplyr::pull(weights, riverID) %>%
        match(rIDs)
    wgIDs <- dplyr::pull(weights, zoneID) %>%
        match(gIDs)
    weightvec <- dplyr::pull(weights, weights)
    
    gridareas <- sf::st_area(grid) %>% units::set_units("m2")
        
    
    runoff_ts <- collect_listc(grid$runoff_ts)
    ngrids <- length(runoff_ts)
    unidates <- lapply(grid$runoff_ts, function(x) x$Date) %>%
        unlist %>%
        unique %>%
        lubridate::as_date()
    
    total <- ngrids
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    #---------------------------------------------------------------------------
    # process
    
    qts <- list()
    for (g in 1:ngrids) {
        
        names <- colnames(runoff_ts[[g]])[-1]
        nts <- nrow(runoff_ts[[g]])
        runoffTS <- runoff_ts[[g]]
        
        QTS <- matrix(0, nrow = nts, ncol = nriv) 
        
        
        unit <- units::deparse_unit(runoffTS)
        if(unit != "m3 s-1") {
            runoffTS <- unit_conversion(runoffTS, 
                                        unit,
                                        gridareas[rep(1:ng, each = nts)])
        } 
        
        # remove units after conversion to speed up computation, and add them back
        # later on.
        runoffTS <- units::drop_units(runoffTS)
        
        for (seg in 1:nseg) {
            if (is.na(wrIDs[seg])) next
            QTS[, wrIDs[seg] ] <- QTS[, wrIDs[seg] ] + 
                weightvec[seg] *
                runoffTS[, wgIDs[seg] ]
        }

        QTS <- units::set_units(QTS, "m3/s")
        QTS <- dplyr::as_tibble(QTS, .name_repair = "minimal")
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
downscale_pycno <- function(HS, n, dasy = NULL, verbose = FALSE) {
    
    outID <- NULL
    orig_id <- NULL
    riverID <- NULL
    geom <- NULL
    convert <- NULL
    
    if(verbose) message("Preprocessing..")
    
    # if (unit == "mm/s") convert <- TRUE
    # if (unit == "m3/s") convert <- FALSE
    
    # ##################
    # # create padding
    # outer_buffer <- sf::st_union(HS$weights) %>%
    #     st_cast("POLYGON") %>%
    #     sf::st_cast("LINESTRING") %>%
    #     sf::st_buffer(dist = 0.01)
    # 
    # # break padding by centroids of neighbours, join attributes
    # outer_basin_centroids <- sf::st_intersection(HS$weights, outer_buffer) %>%
    #     sf::st_centroid() 
    # 
    # outer_basin <- outer_basin_centroids %>%
    #     sf::st_union() %>%
    #     sf::st_voronoi() %>%
    #     sf::st_cast() %>%
    #     sf::st_sf() %>%
    #     sf::st_join(outer_basin_centroids) %>%
    #     sf::st_intersection(outer_buffer) %>%
    #     sf::st_difference(st_union(HS$weights)) %>%
    #     dplyr::mutate(outID = 1:nrow(.), 
    #                   orig_id = riverID,
    #                   riverID = NA) %>%
    #     dplyr::select(outID, orig_id, riverID, zoneID, dplyr::everything())
    # 
    # ####
    # prepare for pp
    # pycno <- rbind(outer_basin, 
    #                HS$weights %>% 
    #                    tibble::add_column(outID = NA, orig_id = NA) %>%
    #                    dplyr::select(outID, orig_id, riverID, 
    #                                  dplyr::everything()) %>%
    #                    dplyr::rename(geometry = geom))
    
    pycno <- HS$weights %>% 
                       tibble::add_column(outID = NA, orig_id = NA) %>%
                       dplyr::select(outID, orig_id, riverID, 
                                     dplyr::everything()) %>%
                       dplyr::rename(geometry = geom)
    
    #identify neighbours
    touching <- sf::st_touches(pycno)
    
    #identify boundary
    boundary <- sf::st_touches(pycno, 
                               sf::st_union(pycno) %>%
                                   sf::st_cast("POLYGON") %>%
                                   sf::st_cast("LINESTRING"),
                               sparse = FALSE) %>%
        as.numeric()
    boundary[boundary == 0] <- NA
    
    #prep
    nmod <- ncol(HS$source$runoff_ts[[1]])
    nstep <- nrow(HS$source$runoff_ts[[1]])
    names <- colnames(HS$source$runoff_ts[[1]])
    nriver <- nrow(HS$target)
    rivers <- which(!is.na(pycno$riverID))
    gridareas <- sf::st_area(HS$source)
    unidates <- lapply(HS$source$runoff_ts, function(x) x$Date) %>%
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
        colnames(qts) <- HS$target$riverID
        name <- names[model]
        
        for(tstep in 1:nstep) {
            
            r_grid <- sapply(HS$source$runoff_ts, 
                             function(x) dplyr::pull(x[tstep, model]))
            
            r_pycno <- iterate_pycno(pycno, dasy, touching, boundary,
                                     r_grid, gridareas, n, convert=TRUE)

            if(convert) r_pycno <- r_pycno * pycno$target_area / 1000
            
            for(i in seq_along(r_pycno)) {
                where <- which(HS$target$riverID == pycno$riverID[i])
                if(length(where) == 0) next
                qts[tstep, where] <- sum(qts[tstep, where], 
                                         r_pycno[i],
                                         na.rm=TRUE) 
            }
            
        }
        
        qts <- data.frame(qts)
        colnames(qts) <- HS$target$riverID
        qts <- tibble::add_column(qts, Date = unidates, .before=1)
        QTS[[ name ]] <- qts
        
        if (verbose) setTxtProgressBar(pb, model)
    }
    close(pb)
    if (verbose) message("Processing output..")
    
    return(QTS)
    
}


iterate_pycno <- function(p_obj, dasy = NULL, touching, boundary, 
                          r_grid, gridareas, n, convert=TRUE) {
    
    # prepare
    r_orig <- rep(NA, length(touching))
    for(i in seq_along(r_grid)) {
        ind <- which(p_obj$zoneID == as.numeric(names(r_grid)[i]))
        r_orig[ind] <- r_grid[[i]]
    }
    r_prev <- r_orig
    r_curr <- r_orig
    
    iter <- which(!is.na(p_obj$riverID))
    remove <- which(is.na(p_obj$riverID))
    p_obj$zoneID[remove] <- NA
    
    # iterate
    for (i in 1:n) {
        
        for(j in iter) {
            ind <- c(j, touching[[j]])
            boundary_val <- r_orig[j] * boundary[j]
            vals <- c(r_prev[ind], boundary_val)
            new_value <- mean(vals, na.rm=TRUE)
            r_curr[j] <- new_value
        }
        
        # # rescale
            for(j in seq_along(r_grid)) {
                ind <- which(p_obj$zoneID == j)
                vol_c <- r_curr[ind] * p_obj$target_area[ind]
                vol_g <- r_grid[j]*gridareas[j]
                bias <- vol_g / sum(vol_c,na.rm = TRUE)
                vol_c <- vol_c * bias
                
                r_curr[ind] <- vol_c / p_obj$target_area[ind]
            }
        
        r_prev <- r_curr    
    }
    
    # if dasymetric variable is provided
    if(!is.null(dasy)) {
       r_curr <- r_curr * dasy
        
        # same as above
        for(j in seq_along(r_grid)) {
            ind <- which(p_obj$zoneID == j)
            vol_c <- r_curr[ind] * p_obj$target_area[ind]
            vol_g <- r_grid[j]*gridareas[j]
            bias <- vol_g / sum(vol_c,na.rm = TRUE)
            vol_c <- vol_c * bias
            
            r_curr[ind] <- vol_c / p_obj$target_area[ind]
        }
    }
    
    return(r_curr)
} 


