#' Computes and regionalizes optimal estimate from multiple discharge 
#' timeseries against observed streamflow
#' 
#' Algorithm routes runoff using user specified routing algorithm, for
#' each observation point in sequence starting from the most upstream
#' point. The optimum weight combination is regionalized to the other
#' river segments which do not contain observation information. 
#' 
#' Currently only one regionalization type is implemented: \code{
#' upstream} uses the weights obtained for the nearest downstream
#' station for the river segment.
#' 
#' Options for \code{no_station} is currently only one: \code{"em"}, 
#' which stands for "ensemble mean". For those river segments with no
#' weights assigned from a station, an ensemble mean (each discharge
#' prediction is given equal weight) is computed.
#' 
#' If \code{bias_correction} is set to \code{TRUE}, constant bias 
#' correction is applied. Bias is divided equally among all river
#' segments which are being optimised within a region. For example,
#' if the sub-basin being optimised has 10 river segments, and bias
#' (mean error) at the station being optimised is 10 m3/s, each
#' river segment is added with a constant 1 m3/s runoff.
#' 
#' @param HSrunoff An \code{HSrunoff} object containing  runoff
#'   estimates.
#' @param HSobs An \code{HSobs} object containing the observed timeseries
#'   for all stations.
#' @param routing Routing algorithm to use. See 
#'   \code{\link{accumulate_runoff}} for options.
#' @param region_type How to regionalize combination weights. See details.
#' @param no_station How to handle river segments with no downstream 
#'   stations. See details.
#' @param bias_correction Whether to apply bias correction. Default FALSE.
#'   If TRUE, constant bias correction is applied to all river segments.
#'   See details.
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
optimise_region <- function(HSrunoff, 
                           HSobs, 
                           routing = "simple", 
                           train = 0.5,
                           optim_method = "CLS",
                           bias_correction=FALSE,
                           region_type='upstream', 
                           no_station = "em", 
                           ...,
                           verbose = FALSE) {

    start <- Sys.time()
    if(verbose) message("Initializing..")
    river <- HSrunoff$river
    
    statrIDs <- HSobs$riverIDs
    nrunoff <- length(HSrunoff$downscaled)
    
    fnames <- names(HSrunoff$downscaled)
    stat_names <- colnames(HSobs$Observation[,-1])
    
    # optimisation only for dates which are present in all downscaled runoff series
    optimdates <- lapply(HSrunoff$downscaled, FUN = function(x) x$Date)
    alldates <- do.call(c,optimdates) %>% 
        unique() %>% 
        lubridate::as_date() %>% 
        data.frame(Date = .)
    optimdates <- Reduce(intersect, optimdates) %>% 
        lubridate::as_date()
    
    
    
    # find nearest up- and downstream station for each station
    rind <- which(river$riverID %in% statrIDs) 
    rind <- suppressMessages(data.frame(riverID = river$riverID[rind], 
                       rind=rind, 
                       upseg = river$UP_SEGMENTS[rind]) %>%
        dplyr::left_join(data.frame(station = names(statrIDs), 
                                                     riverID = statrIDs)))
    rind <- rind[order(rind$upseg),]
    upstations <- data.frame(station = rind$station, up = NA, down = NA)
    for (i in seq_along(rind$station)) {
        ds <- river$DOWNSTREAM[[rind$rind[i]]] 
        temp <- which(river$riverID[rind$rind] %in% ds)
        upstations$down[i] <- rind$station[temp[1]]
        upstations$up[temp[1]] <- rind$station[i]
    }
    upstations <- suppressMessages(dplyr::left_join(upstations, rind))
    
    outdischarge <- alldates
    optimizedIDs <- data.frame(riverID = NA, 
                               OPTIMIZED_STATION = NA, 
                               OPTIMIZED_riverID = NA)
    comb <- list()
    boundary <- NULL
    if (verbose) message("Optimising river flow..")
    if (verbose) pb <- txtProgressBar(min = 0, max = nrow(upstations)+1, style = 3)
    # route station one by one
    for(station in 1:nrow(upstations)) {
        
        # choose only upstream
        statriver <- hydrostreamer::upstream(river, upstations$riverID[station])
        statriver <- statriver[!statriver$riverID %in% optimizedIDs$riverID,]
        
        statrunoff <- HSrunoff
        statrunoff$river <- statriver
        statrunoff$downscaled <- lapply(statrunoff$downscaled, 
                                        FUN=function(x) {
                                            x[x$Date %in% optimdates, 
                                              c("Date",
                                                as.character(statriver$riverID)
                                                )
                                              ]
                                            }
                                        )
        # route
        statflow <- accumulate_runoff(statrunoff, 
                                      method = routing, 
                                      boundary = boundary,
                                      ...)
        
        # combine
        combdates <- optimdates %in% HSobs$Observations$Date
        combdates <- optimdates[combdates]
        statpreds <- lapply(statflow$discharge, 
                            FUN=function(x) x[x$Date %in% combdates,
                                              as.character(upstations$riverID[station])])
        statpreds <- do.call(cbind, statpreds)
        statobs <- HSobs$Observations[HSobs$Observations$Date %in% combdates,
                                      as.character(upstations$station[station])] %>%
            unlist()
        keep <- !is.na(statobs) 
        statobs <- unlist(statobs[keep])
        statpreds <- statpreds[keep,]
        
        train_ <- 1:(round(length(statobs)*train, 0))
        test_ <- (max(train_)+1):length(statobs)
        
        fcast <- suppressMessages(ForecastComb::foreccomb(statobs[train_], 
                                                          statpreds[train_,], 
                                                          statobs[test_], 
                                                          statpreds[test_,]))
        
        name <- as.character(upstations$station[station])
        if(optim_method == "factorCLS") {
            # This is the function ForecastComb::comb_CLS() in a factorized form 
            # because the original unfactorized ForecastComb function results often 
            # in no solutions error in solve.QP(). The fix is sourced from 
            # StackOverflow: https://stackoverflow.com/a/28388394
            comb[[name]] <- forecastcomb_comb_CLS(fcast) 
        } else if (optim_method == "CLS") {
            comb[[name]] <- ForecastComb::comb_CLS(fcast)
        } else if (optim_method == "OLS") {
            comb[[name]] <- ForecastComb::comb_OLS(fcast)
        } 
        
        # extract coefficients (weights) and intercept, and bias correction
        weights <- comb[[station]]$Weights
        names(weights) <- comb[[station]]$Models
        
        
        if(is.null(comb[[station]]$Intercept)) {
            intercept <- 0
        } else{
            intercept <- comb[[station]]$Intercept / nrow(statflow$river)
        }
        
        if (bias_correction) {
            bias <- comb[[station]]$Accuracy_Train[1] / nrow(statflow$river)
        } else {
            bias <- 0
        }
        
        # compute optimized discharge
        mod <- names(weights)
        optimflow <- statflow$discharge[[ mod[1] ]][, -1] *
            weights[ mod[1] ] +
            intercept + 
            bias
        optimflow <- data.frame(Date = statflow$discharge[[ mod[1] ]][,"Date"], 
                                optimflow) 
        colnames(optimflow) <- colnames(statflow$discharge[[ mod[1] ]])
        n <- ncol(optimflow)
        for (rts in 2:length(mod)) {
                temp <- statflow$discharge[[ mod[rts] ]][, -1] * 
                    weights[ mod[rts] ] +
                    intercept +
                    bias
                optimflow[,2:n] <- optimflow[,2:n] + temp
        }
        
        outdischarge <- suppressMessages(dplyr::left_join(outdischarge, 
                                                          optimflow,
                                                          by="Date"))
        optimizedIDs <- rbind(optimizedIDs, 
                              data.frame(riverID = statriver$riverID, 
                                         OPTIMIZED_STATION=as.character(upstations$station[station]),
                                         OPTIMIZED_riverID = upstations$riverID[station]))
        
        
        if (station == 1) {
            optimizedIDs <- optimizedIDs[2:nrow(optimizedIDs),]
            boundary <- create_HSobs(optimflow[,c("Date", 
                                                  as.character(upstations$riverID[station]) )],
                                        river$NEXT[upstations$rind[station]])
            colnames(boundary$Observations) <- c("Date", boundary$riverIDs)
        } else {
            boundary$Observations <- suppressMessages(dplyr::left_join(
                boundary$Observations, 
                optimflow[,c("Date", 
                             as.character(upstations$riverID[station]) )],
                by = "Date"))
            boundary$riverIDs <- c(boundary$riverIDs, river$NEXT[upstations$rind[station]])
            colnames(boundary$Observations) <- c("Date", boundary$riverIDs)
        }
        if (verbose) setTxtProgressBar(pb, station)
    }
    
    
    # after all stations are optimized, process the remaining segments
    
    if(no_station == "em") {
        # choose only upstream
        statriver <- river
        statriver <- statriver[!statriver$riverID %in% optimizedIDs$riverID,]
        
        statrunoff <- HSrunoff
        statrunoff$river <- statriver
        statrunoff$downscaled <- lapply(statrunoff$downscaled, 
                                        FUN=function(x) x[x$Date %in% optimdates, 
                                                          c("Date",
                                                            as.character(statriver$riverID))])
        # route
        statflow <- accumulate_runoff(statrunoff, 
                                      method = routing, 
                                      boundary = boundary,
                                      ...)

        
        mod <- names(statrunoff$downscaled)
        weights <- 1/length(mod)
        optimflow <- statflow$discharge[[ mod[1] ]][, -1] * weights
        optimflow <- data.frame(Date = statflow$discharge[[ mod[1] ]][,"Date"], 
                                optimflow) 
        colnames(optimflow) <- colnames(statflow$discharge[[ mod[1] ]])
        n <- ncol(optimflow)
        for (rts in 2:length(mod)) {
            temp <- statflow$discharge[[ mod[rts] ]][, -1] * weights
            optimflow[,2:n] <- optimflow[,2:n] + temp
        }

        
        outdischarge <- suppressMessages(dplyr::left_join(outdischarge, 
                                                          optimflow,
                                                          by = "Date"))
        optimizedIDs <- rbind(optimizedIDs, 
                              data.frame(riverID = statriver$riverID, 
                                         OPTIMIZED_STATION = "Ensemble Mean",
                                         OPTIMIZED_riverID = NA))
        
    }
    
    if (verbose) setTxtProgressBar(pb, station+1)
    if (verbose) close(pb) 
    
    #temp <- as.numeric(colnames(outdischarge)[-1])
    ord <- match(as.character(river$riverID), colnames(outdischarge)[-1])
    
    river <- suppressMessages(dplyr::left_join(river, optimizedIDs))
    output <- list(river = river, 
                   discharge = list(optimized = outdischarge[,c(1,ord+1)]),
                   optim_info = comb)
    class(output) <- c("HSflow", class(output))
    if (verbose) message(paste0("Finished in ", Sys.time()-start))
    return(output)
    
}
