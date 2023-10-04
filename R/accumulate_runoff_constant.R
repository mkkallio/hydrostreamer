#' Apply constant velocity river routing
#' 
#' Implements a simple constant velocity routing algorithm which can be used 
#' with arbitrary timesteps.
#'
#' @param velocity Flow velocity. Can be a constant, or a vector of flow 
#'   velocity at each unique river segments. Defaults to 1 meter per second.
#' @inheritParams accumulate_runoff_instant
#'
#' @return Returns the input object \code{HS}) with an added list column
#'   \code{discharge_ts} containing routed discharge estimates for each river
#'    segment. 
#' 
#' @export 
accumulate_runoff_constant <- function(HS, 
                                       velocity = 1,
                                       verbose=FALSE) {
    
    riverID <- NULL
    NEXT <- NULL
    PREVIOUS <- NULL
    UP_SEGMENTS <- NULL
    
    route <- "forward" # the only option currently available
    
    test <- inherits(HS, "HS")
    if(!test) stop("HS must be of class HS")
    
    ###########################
    # PREPROCESSING
    
    if(verbose) message("Preparing...")
    
    # routing does not work for river networks of 1 segment: duplicate and
    # remove duplicate at the end
    test <- nrow(HS) == 1
    if(test) {
        single_segment <- TRUE
        HS <- rbind(HS, HS)
        HS$riverID[2] <- -9999
    } else {
        single_segment <- FALSE
    }
    
    
    
    #prepare required variables
    lengths <- sf::st_length(HS) %>% units::drop_units()
    IDs <- dplyr::pull(HS, riverID)
    
    order <- HS %>%
        dplyr::select(riverID, UP_SEGMENTS) %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::arrange(UP_SEGMENTS) %>%
        dplyr::select(riverID) %>%
        unlist() %>%
        match(IDs)
    
    ## find next river
    ind <- find_attribute(HS, "next_col", TRUE)
    nextriver <- dplyr::pull(HS, ind) %>%
        match(IDs)
    
    duration <- lengths/velocity
    
    # timeserie
    flow <- HS$runoff_ts
    names(flow) <- HS$riverID
    unit <- units::deparse_unit(dplyr::pull(flow[[1]], 2))
    nseg <- length(order)
    preds <- ncol(flow[[1]])-1
    
    # dates and time intervals between dates
    dates <- lapply(flow, function(x) x$Date) %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        lubridate::as_date()
    intervals <- vector("numeric", length(dates))
    for (i in seq_along(intervals)) {
        if (i == length(intervals)) {
            intervals[i] <- lubridate::interval(dates[i],
                                                lubridate::ceiling_date(dates[i],
                                                                        unit="month")) / 
                lubridate::seconds(1)
        } else {
            intervals[i] <- lubridate::interval(dates[i],
                                                dates[i+1]) / lubridate::seconds(1)
        }
    }
    
    # test intervals
    test <- max(duration) > min(intervals)
    maxlen <- min(intervals)*velocity
    if(test) stop("Constant routing does not currently support river ",
                  "segments longer than through which water passes ",
                  "during a single timestep. Consider breaking the ",
                  "longest segments into smaller pieces. With the ",
                  "current velocity, maximum segment length is ", 
                  maxlen, " meters.")
    
    
    # inspect downstream for each segment
    downstream <- list()
    temp <- dplyr::select(HS, riverID, NEXT, PREVIOUS) %>%
        tibble::add_column(duration = duration) %>%
        sf::st_drop_geometry()
    
    for(i in order) {
        downstream[[i]] <- downstream(temp, temp$riverID[i]) %>%
            dplyr::mutate(cumulative = cumsum(duration))
    }
    
    
    downstreamind <- lapply(downstream, function(x) match(x$riverID, 
                                                          names(flow)))
    
    # determine size of padding needed
    longest_duration <- max(unlist(sapply(downstream, 
                                   function(x) x$cumulative)))
    pad_n <- ceiling(longest_duration / min(intervals)) +1
    
    # dates with padding
    dates_padded <- c(rep(NA, pad_n), dates, rep(NA, pad_n))
    

    
    # process control timeseries?
    test <- hasName(HS, "control_ts")
    if(test) {
        boundary_runoff <- unname(which(sapply(HS$control_type, function(x) {
            if(is.null(x)) {
                return(FALSE)
            } else {
                return(x[2] == "runoff")
            }
        })))
        if(length(boundary_runoff) == 0) {
            rboundary <- FALSE
        } else rboundary <- TRUE
        boundary_discharge <- unname(which(sapply(HS$control_type, function(x) {
            if(is.null(x)) {
                return(FALSE)
            } else {
                return(x[2] == "discharge")
            }
        })))
        if(length(boundary_discharge) == 0) {
            dboundary <- FALSE
        } else dboundary <- TRUE
    } else {
        rboundary <- FALSE
        dboundary <- FALSE
    }
    
    
    
    ################################
    # ROUTE
    # create array, and pad and populate it
    nts <- nrow(flow[[1]])+pad_n
    inflowmat <- outflowmat <- array(0,dim=c(nts+pad_n,nseg,preds))
    
    for(seg in 1:nseg) {
        Qin <- as.matrix(flow[[seg]][,-1])
        Qin <- rbind(matrix(rep(Qin[1,],pad_n), ncol=preds, byrow=TRUE,
                            dimnames = list(NULL,colnames(Qin))),
                     Qin,
                     matrix(rep(Qin[nrow(Qin),], pad_n), ncol=preds, byrow=TRUE,
                            dimnames = list(NULL,colnames(Qin))))
        
        # apply boundary for runoff
        if(rboundary) {
            test <- seg %in% boundary_runoff
            if(test) {
                type <- HS$control_type[[seg]][1]
            
                inds <- dates_padded %in% HS$control_ts[[seg]]$Date
                cts <- units::drop_units(dplyr::pull(HS$control_ts[[seg]], 2))
                
                if(type == "add") {
                    Qin[inds,] <- Qin[inds,] + cts
                } else if(type == "subtract") {
                    Qin[inds,] <- Qin[inds,] - cts
                } else if(type == "multiply") {
                    Qin[inds,] <- Qin[inds,] * cts
                } else if(type == "set") {
                    Qin[inds,] <- 0
                    Qin[inds,] <- Qin[inds,] + cts
                } 
            }
        }
            
        inflowmat[,seg,] <- Qin
    }
    inflownas <- is.na(inflowmat)
    inflowmat[inflownas] <- 0
    
    #pad also intervals
    interval <- c(rep(intervals[[1]], pad_n), intervals)
    
    # --------------------------------------------------------------------------
    # ROUTE FORWARD
    if(verbose) message("Routing...")
    max <- preds
    if (verbose) pb <- txtProgressBar(min = 0, max = max, style = 3)
    prog <- 0
    
    
    if(route == "forward") {
        
        # ----------------------------------------------------------------------
        # record contribution
       
        record <- record_forward(downstream, 
                                 downstreamind, 
                                 interval)
        
        # ----------------------------------------------------------------------
        # route
        # tsinds <- (pad_n+1):(nrow(inflowmat)-pad_n)
        for(pred in 1:preds) {
            
            outflowmat[,,pred] <- constantroute(inflowmat[,,pred], record, 
                                                pad_n, nseg)

            if(verbose) setTxtProgressBar(pb, pred)
        }
    }
    outflowmat[inflownas] <- NA
    
    # apply discharge boundary
    if(dboundary) {
        for(seg in boundary_discharge) {
            type <- HS$control_type[[seg]][1]
            
            inds <- which(dates_padded %in% HS$control_ts[[seg]]$Date)-pad_n
            cts <- units::drop_units(dplyr::pull(HS$control_ts[[seg]], 2))
            
            if(type == "add") {
                outflowmat[inds,seg,] <- outflowmat[inds,seg,] + cts
            } else if(type == "subtract") {
                outflowmat[inds,seg,] <- outflowmat[inds,seg,] - cts
            } else if(type == "multiply") {
                outflowmat[inds,seg,] <- outflowmat[inds,seg,] * cts
            } else if(type == "set") {
                outflowmat[inds,seg,] <- 0
                outflowmat[inds,seg,] <- outflowmat[inds,seg,] + cts
            } else {
                stop("Error in boundary operation type.")
            }
        }
    }
    
    if(verbose) close(pb)
    
    # --------------------------------------------------------------------------
    # ROUTE REVERSE
    # NOT IMPLEMENTED YET
    if(route == "reverse") {

    }
    
    
    # --------------------------------------------------------------------------
    # prepare output
    
    outflowmat <- units::as_units(outflowmat, "m3 s-1")
    for(seg in 1:nseg) {
        out <- flow[[seg]]
        nas <- is.na(out)
        #out[,-1] <- outflowmat[(pad_n+1):(tsteps),seg,]
        for(i in 1:preds) {
            out[,i+1] <- outflowmat[1:nrow(out), seg, i]
        }
        out[nas] <- NA
        out <- dplyr::as_tibble(out, .name_repair = "minimal")
        flow[[seg]] <- out
    }
    
    output <- HS 
    output$discharge_ts <- flow
    
    if(single_segment) output <- output[1,]
    
    output <- reorder_cols(output)
    output <- assign_class(output, "HS")
    return(output)
}










record_forward <- function(downstream, downstreamind, interval) {
    
    n <- length(downstream)
    record <- lapply(1:n, function(x) {
        list(shares = NULL,
             tsteps = NULL,
             segment = NULL)
    })
    interval <- mean(interval)
    for(seg in 1:n) {
        
        
        # get indices of downstream segments
        ds <- downstreamind[[seg]]
        
        # inflowing runoff at segment seg, timestep ts
        q <- 1
        
        # cumulative time through all downstream segments standardized by
        # the interval between timesteps
        cumulative <- downstream[[seg]]$cumulative / interval
        tf <- floor(cumulative)
        tfuni <- unique(tf)
        
        # Identify last segment water flows through in each segment
        last <- c(0, cumsum(rle(tf)$lengths))
        
        outflow <- matrix(0,
                          nrow = length(tfuni)+1,
                          ncol = length(cumulative))
        
        for(i in 2:length(last)) {
            # first and last segment
            f <- last[i-1]+1
            l <- last[i]
            
            # outflow = q * how long it takes to pass through during
            # the timestep
            outflow[i-1,f:l] <- q*(1-(cumulative[f:l]-tf[f:l]))
            
            # flow that did not flow through the segments during,
            # the timestep, flows through on the next tstep (?)
            outflow[i,f:l] <- q-outflow[i-1,f:l]
        }
        
        for(i in 1:ncol(outflow)) {
            shares <- outflow[,i]
            tsteps <- which(shares != 0)
            shares <- shares[tsteps]
            segment <- ds[i]
            rec <- record[[segment]]
            rec$shares <- c(rec$shares, rev(shares))
            rec$tsteps <- c(rec$tsteps, as.integer(rev(tsteps-1)))
            rec$segment <- c(rec$segment, rep(seg, length(shares)))
            record[[segment]] <- rec
        }
    }
    return(record)
}

record_reverse <- function(downstream, downstreamind, interval, inflowmat) {
    
    nseg <- NULL
    
    record <- lapply(1:nseg, function(x) {
        list(shares = NULL,
             tsteps = NULL,
             segment = NULL)
    })
    interval <- mean(interval)
    for(seg in 1:nseg) {
        
        
        # get indices of downstream segments
        ds <- downstreamind[[seg]]
        
        # inflowing runoff at segment seg, timestep ts
        q <- 1
        
        # cumulative time through all downstream segments standardized by
        # the interval between timesteps
        cumulative <- downstream[[seg]]$cumulative / interval
        tf <- floor(cumulative)
        tfuni <- unique(tf)
        
        # Identify last segment water flows through in each segment
        last <- c(0, cumsum(rle(tf)$lengths))
        
        outflow <- matrix(0,
                          nrow = length(tfuni)+1,
                          ncol = length(cumulative))
        
        for(i in 2:length(last)) {
            # first and last segment
            f <- last[i-1]+1
            l <- last[i]
            
            # outflow = q * how long it takes to pass through during
            # the timestep
            outflow[i-1,f:l] <- q*(1-(cumulative[f:l]-tf[f:l]))
            
            # flow that did not flow through the segments during,
            # the timestep, flows through on the next tstep (?)
            outflow[i,f:l] <- q-outflow[i-1,f:l]
        }
        
        for(i in 1:ncol(outflow)) {
            shares <- outflow[,i]
            tsteps <- which(shares != 0)
            shares <- shares[tsteps]
            segment <- ds[i]
            rec <- record[[segment]]
            rec$shares <- c(rec$shares, rev(shares))
            rec$tsteps <- c(rec$tsteps, as.integer(rev(tsteps-1)))
            rec$segment <- c(rec$segment, rep(seg, length(shares)))
            record[[segment]] <- rec
        }
    }
    return(record)
}





# apply_boundary <- function(control_ts, discharge, type) {
#     # check and apply controls condition
#     
#     dateind <- discharge$Date %in% control_ts$Date
#     
#     # Set, of modify input runoff of the segment
#     if (type == "set") {
#         for(pred in 2:ncol(discharge)) {
#             discharge[dateind,pred] <- control_ts[,2]
#         }
#         
#         # if no downstream segments, go to next seg
#         if(!is.na(nextriver)) {
#             new_dis <- discharge[[nextriver[seg] ]][,-1] + 
#                 discharge[,-1]
#             
#             discharge[[ nextriver[seg] ]][,-1] <- new_dis
#         }
#         
#     } else if (type == "add") {
#         for(pred in 2:ncol(discharge)) {
#             discharge[dateind,pred] <- 
#                 discharge[dateind,pred] + control_ts[,2]
#         }
#         
#     } else if (type == "subtract") {
#         for(pred in 2:ncol(discharge)) {
#             discharge[dateind,pred] <- 
#                 discharge[dateind,pred] - control_ts[,2]
#         }
#         
#     } else if (type == "multiply") {
#         for(pred in 2:ncol(discharge)) {
#             discharge[dateind,pred] <- 
#                 discharge[dateind,pred] * control_ts[,2]
#         }
#     }
#     return(discharge)
# }

