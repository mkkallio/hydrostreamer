#' Computes and regionalizes optimal estimate from multiple discharge 
#' timeseries against observed streamflow
#' 
#' Algorithm routes runoff using user specified routing algorithm, for
#' each observation point in sequence starting from the most upstream
#' point. The optimum weight combination is regionalized to the river segments 
#' upstream from the optimisation point which do not contain observation 
#' information. 
#' 
#' Currently only one regionalization type is implemented: \code{
#' upstream} uses the weights obtained for the nearest downstream
#' station for the river segment.
#' 
#' Options for \code{no_station} is currently only one: \code{"em"}, 
#' which stands for "ensemble mean". For those river segments with no
#' weights assigned from a station (segments with no downstream observation 
#' station), an ensemble mean (each discharge prediction is given equal weight) 
#' is computed.
#' 
#' 
#' @param HS An \code{HS} object containing runoff
#'   estimates.
#' @param routing Routing algorithm to use. See 
#'   \code{\link{accumulate_runoff}} for options.
#' @param region_type How to regionalize combination weights. See details.
#' @param no_station How to handle river segments with no downstream 
#'   stations. See details.
#' @param bias_correction Whether to apply bias correction. Default FALSE.
#' @param drop Drop existing timeseries (columns) in \code{runoff_ts}, 
#'   \code{discharge_ts}, or not.
#' @param ... Additional parameters passed to the \code{routing} algorithm.
#' @inheritParams optimise_point
#' @inheritParams compute_HSweights
#' 
#' @return Returns an \code{HSflow} object with one discharge layer (the
#'   optimized streamflow), and two additional columns in 
#'   \code{HSflow$river}:
#'   \itemize{
#'     \item OPTIMIZED_STATION: Name of the station from HSobs which'
#'       optimized weights were used at the river segment
#'     \item OPTIMIZED_riverID: ID of the segment the weighted station
#'       is located in.
#'   }
#'   
#' @export
optimise_region <- function(HS, 
                            routing = "simple", 
                            train = 0.5,
                            optim_method = "CLS",
                            bias_correction=FALSE,
                            combination = "ts",
                            sampling = "series",
                            region_type= "upstream", 
                            no_station = "em", 
                            drop = TRUE,
                            ...,
                            verbose = FALSE) {
    
    . <- NULL
    upseg <- NULL
    control_type <- NULL
    control_ts <- NULL
    warned_overfit <- FALSE
    warned_train <- FALSE
    
    
    start <- Sys.time()
    if(verbose) message("Initializing..")
    
    statrIDs <- lapply(HS$observation_ts, is.null)
    statrIDs <- which(!unlist(statrIDs))
    stat_names <- HS$observation_station[ statrIDs ]
    
    # find nearest up- and downstream station for each station
    rind <- statrIDs 
    rind <- data.frame(riverID = HS$riverID[statrIDs], 
                       rind=statrIDs, 
                       upseg = HS$UP_SEGMENTS[statrIDs],
                       station = stat_names) %>%
        dplyr::arrange(upseg)
    
    
    upstations <- data.frame(station = rind$station, up = NA, down = NA)
    
    for (i in seq_along(rind$station)) {
        ds <- downstream(HS, rind$riverID[i])$riverID
        
        temp <- which(HS$riverID[rind$rind] %in% ds)
        upstations$down[i] <- rind$station[temp[2]]
        upstations$up[temp[2]] <- rind$station[i]
    }
    upstations <- suppressMessages(dplyr::left_join(upstations, rind))
    optimizedIDs <- data.frame(riverID = NA, 
                               OPTIMIZED_STATION = NA, 
                               OPTIMIZED_riverID = NA)
    
    if (verbose) {
        message("Optimising river flow..")
        pb <- txtProgressBar(min = 0, max = nrow(upstations)+1, style = 3)
    }
    
    HS$Optimisation_info <- vector("list", nrow(HS))
    HS$Optimised_at <- rep(NA, nrow(HS))
    HS$discharge_ts <- vector("list", nrow(HS))
    HS <- HS[order(HS$riverID),]
    
    # record original control timeseries
    controltype <- NULL 
    if(hasName(HS, "control_type")) controltype <- HS$control_type
    controlts <- NULL
    if(hasName(HS, "control_ts")) controlts <- HS$control_ts
    
    # route and combine stations one by one
    for(station in 1:nrow(upstations)) {
        
        # choose only upstream
        statriver <- upstream(HS, upstations$riverID[station])
        statriver <- statriver[!statriver$riverID %in% optimizedIDs$riverID,]
        statriver <- statriver[order(statriver$riverID),]
        
        # route
        statriver <- accumulate_runoff(statriver, 
                                       method = routing,
                                       ...)
        
        # prepare combination
        stationrID <- which(statriver$riverID == upstations$riverID[station])
        flow <- dplyr::left_join(statriver$discharge_ts[[ stationrID ]],
                                 statriver$observation_ts[[ stationrID ]],
                                 by="Date")
        
        colremove <- apply(flow, 2, FUN=function(x) all(is.na(x)))
        if(any(colremove)) {
            flow <- flow[,names(colremove)[!colremove]]
        }
        
        ############################################
        # Forecast combination entire timeseries or monthly
        if(combination %in% c("timeseries", "ts")) {
            
            comb <- combine_timeseries(tibble::as_tibble(flow), 
                                                       optim_method, 
                                                       sampling,
                                                       train,
                                                       bias_correction,
                                                       warned_overfit,
                                                       warned_train)
            warned_overfit <- comb$warned_overfit
            warned_train <- comb$warned_train
            
            # # get the optimized weights and combine
            # # with intercept and bias divided among river segments
            # if (bias_correction) {
            #     intbias <- ( comb$Intercept + comb$Bias_correction ) /
            #         nrow(statriver)
            #     
            # } else {
            #     intbias <- comb$Intercept / nrow(statriver)
            # }
            # weights <- c(Intercept = intbias, comb$Weights) 
            
            # # get the optimized weights and combine
            weights <- c(Intercept = comb$Intercept, comb$Weights) 
            
            if (bias_correction) {
                bias <- comb$Bias_correction
            } else {
                bias <- NULL
            }
            
            statriver <- combine_runoff(statriver, 
                                        list(Optimized = weights),
                                        bias = comb$Bias_correction,
                                        drop = drop,
                                        monthly = FALSE)
            
        } else if(combination %in% c("monthly", "mon")) {
            
            comb <- combine_monthly(tibble::as_tibble(flow), 
                                                    optim_method, 
                                                    sampling,
                                                    train,
                                                    bias_correction,
                                                    warned_overfit,
                                                    warned_train)
            warned_overfit <- comb$warned_overfit
            warned_train <- comb$warned_train
            
            # get the optimized weights and combine
            weights <- cbind(Intercept = comb$Intercept, comb$Weights)
            
            if(bias_correction) {
                bias <- comb$Bias_correction %>% 
                    as.matrix() %>% 
                    t()
            } else {
                bias <- NULL
            }
            
            
            statriver <- combine_runoff(statriver, 
                                        list(Optimized = weights),
                                        bias = bias,
                                        drop = drop,
                                        monthly = TRUE)
        }
        
        # update comb info
        opt <- comb$Optimized_ts
        opt$Optimized <- statriver$discharge_ts[[stationrID]]$Optimized
        opt$Residuals <- opt$Optimized - opt$Observations
        comb$Optimized_ts <- opt
        
        trains <- comb$Optimized_ts$Train_or_test == "Train"
        tests <- comb$Optimized_ts$Train_or_test == "Test"
        
        if(sum(tests, na.rm=TRUE) == 0) {
            traingof <- hydroGOF::gof(comb$Optimized_ts$Optimized[trains], 
                                      comb$Optimized_ts$Observations[trains])  
            gofs <- data.frame(Train = traingof)
        } else {
            traingof <- hydroGOF::gof(comb$Optimized_ts$Optimized[trains], 
                                      comb$Optimized_ts$Observations[trains]) 
            
            testgof <- hydroGOF::gof(comb$Optimized_ts$Optimized[tests], 
                                     comb$Optimized_ts$Observations[tests])
            
            bothgof <- hydroGOF::gof(comb$Optimized_ts$Optimized, 
                                     comb$Optimized_ts$Observations)
            
            gofs <- data.frame(Train = traingof,
                               Test = testgof,
                               Together = bothgof)
        }
        
        comb$Goodness_of_fit <- gofs
        
        # update HS 
        statriver$Optimisation_info[[stationrID]] <- comb
        statriver$Optimised_at <-
            upstations$riverID[station]
        
        update <- HS$riverID %in% statriver$riverID
        HS$discharge_ts[update] <- statriver$discharge_ts
        HS$runoff_ts[update] <- statriver$runoff_ts
        HS$Optimised_at[update] <- statriver$Optimised_at
        HS$Optimisation_info[update] <- statriver$Optimisation_info
        
        # mark which river segments have already been optimized
        optimizedIDs <- rbind(optimizedIDs, 
                              data.frame(riverID = statriver$riverID, 
                                         OPTIMIZED_STATION=upstations$station[station],
                                         OPTIMIZED_riverID = upstations$riverID[station]))
        
        # create a boundary conditions
        #boundaryrID <- 
        #    which(HS$riverID == HS$NEXT[upstations$rind[station]])
        boundaryrID <- HS$NEXT[upstations$rind[station]]
        
        if (boundaryrID != -9999) {
            boundary <- statriver$discharge_ts[[stationrID]] 
            HS <- add_control(HS, boundary, boundaryrID, "add")
        }
        
        
        if (verbose) setTxtProgressBar(pb, station)
    }
    
    
    # after all stations are optimized, process the remaining segments
    
    if(no_station == "em") {
        
        
        # choose only segments which are left
        statriver <- HS[!HS$riverID %in% optimizedIDs$riverID,]
        statriver <- statriver[order(statriver$riverID),]
        
        
        # route
        statriver <- accumulate_runoff(statriver, 
                                       method = routing,
                                       ...)
        
        # mean combination
        
        statriver <- ensemble_summary(statriver, 
                                      summarise_over_timeseries = FALSE,
                                      aggregate_monthly = FALSE,
                                      funs = "mean",
                                      drop=TRUE)
        
        # update HS 
        for(i in seq_along(statriver$Optimisation_info)) {
            statriver$Optimisation_info[[i]] <- 
                "Ensemble Mean - not optimized"
        }
        statriver$Optimised_at <- NA
        
        update <- HS$riverID %in% statriver$riverID
        HS$discharge_ts[update] <- statriver$discharge_ts
        HS$runoff_ts[update] <- statriver$runoff_ts
        HS$Optimised_at[update] <- statriver$Optimised_at
        HS$Optimisation_info[update] <- statriver$Optimisation_info
        
        # mark which river segments have already been optimized
        optimizedIDs <- rbind(optimizedIDs, 
                              data.frame(riverID = statriver$riverID, 
                                         OPTIMIZED_STATION = "Ensemble Mean",
                                         OPTIMIZED_riverID = NA))
        
    }
    
    if (verbose) setTxtProgressBar(pb, station+1)
    if (verbose) close(pb) 
    
    # return original control timeseries, if HS had any. Otherwise remove
    if(is.null(controltype)) {
        HS <- dplyr::select(HS, -control_type)
    } else {
        HS$control_type <- controltype 
    }
    if(is.null(controlts)) {
        HS <- dplyr::select(HS, -control_ts)
    } else {
        HS$control_ts <- controlts
    }
    
    # give names to runoff and discharge cols
    names(HS$runoff_ts) <- HS$riverID
    names(HS$discharge_ts) <- HS$riverID
    
    # return
    HS <- reorder_cols(HS)
    HS <- assign_class(HS, "HS")
    if (verbose) message(paste0("Finished in ", Sys.time()-start))
    return(HS)
    
} 









