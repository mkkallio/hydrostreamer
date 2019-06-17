#' Apply simple lag river routing
#' 
#' Implements a simple lag-based routing algorithm which can be used with
#' arbitrary intervals between runoff units. The river flow is 'slowed' down by
#' computing how much water the river segment has at any one time. This 'storage'
#' is added to the next timestep.
#'
#' @inheritParams accumulate_runoff_muskingum
#'
#' @return Returns the input object \code{HS}) with an added list column
#'   \code{discharge_ts} containing routed discharge estimates for each river
#'    segment. 
#' 
#' @export 
accumulate_runoff_simple2 <- function(HS, 
                                     velocity = 1,
                                     verbose=FALSE) {
    
    riverID <- NULL
    UP_SEGMENTS <- NULL
    
    
    ###########################
    # PREPROCESSING
    
    if(verbose) message("Preparing...")
    
    #prepare required variables
    lengths <- sf::st_length(HS) %>% unclass()
    IDs <- dplyr::pull(HS, riverID) # %>% 
    # sf::st_set_geometry(NULL) %>% 
    # unlist()
    
    order <- HS %>%
        dplyr::select(riverID, UP_SEGMENTS) %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::arrange(UP_SEGMENTS) %>%
        dplyr::select(riverID) %>%
        unlist() %>%
        match(IDs)
    
    nextriver <- HS$NEXT %>%
        match(IDs)
    
    duration <- lengths/velocity
    
    # timeserie
    flow <- HS$runoff_ts
    
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


    # process control timeseries?
    if (hasName(HS, "control_ts")) {
        boundary <- TRUE
        boundary_inds <- which(!sapply(HS$control_ts,is.null))
    } else {
        boundary <- FALSE
    }
    
    
    
    ################################
    # ROUTE
    nseg <- length(order)
    preds <- ncol(flow[[1]])-1
    tsteps <- nrow(flow[[1]])+pad_n
    
    # create array, and pad and populate it
    inflowmat <- outflowmat <- array(0,dim=c(tsteps+pad_n,nseg,preds))
    
    for(seg in order) {
        Qin <- as.matrix(flow[[seg]][,-1])
        Qin <- rbind(matrix(rep(Qin[1,],pad_n), ncol=preds, byrow=TRUE,
                            dimnames = list(NULL,colnames(Qin))),
                     Qin,
                     matrix(rep(Qin[nrow(Qin),], pad_n), ncol=preds, byrow=TRUE,
                            dimnames = list(NULL,colnames(Qin))))
        inflowmat[,seg,] <- Qin
    }
    #pad also intervals
    interval <- c(rep(intervals[[1]], pad_n), intervals)
    
    
    if(verbose) message("Routing...")
    max <- preds*nseg
    if (verbose) pb <- txtProgressBar(min = 0, max = max, style = 3)
    prog <- 0
    
    # route each runoff timeseries
    for(pred in 1:preds) {
        
        # prepare for fortran
        inmat <- inflowmat[,,pred]
        # remove NA for fortran
        inmat_na <- is.na(inmat)
        inmat[inmat_na] <- 0
        outmat <- matrix(0, nrow=nrow(inmat), ncol=ncol(inmat))
        
        ds <- NULL
        seg <- NULL
        cumulative <- NULL
        
        # prepare fortran inputs
        fort <- list(tsteps,
                     pad_n,
                     nseg,
                     length(ds),
                     seg,
                     ds,
                     interval,
                     cumulative,
                     inmat,
                     outmat)
        
        for(seg in order) {
            prog <- prog +1
            
            # get indices of downstream segments
            ds <- downstreamind[[seg]]
            cumulative <- downstream[[seg]]$cumulative
            
            # update fortran inputs for this segment
            fort[[3]] <- nseg
            fort[[4]] <- length(ds)
            fort[[5]] <- seg
            fort[[6]] <- ds
            fort[[8]] <- cumulative
            
            # Fortran routine updates the input list
            fort <- .Fortran("routesimple2",
                             as.integer(fort[[1]]),
                             as.integer(fort[[2]]),
                             as.integer(fort[[3]]),
                             as.integer(fort[[4]]),
                             as.integer(fort[[5]]),
                             as.integer(fort[[6]]),
                             as.double(fort[[7]]),
                             as.double(fort[[8]]),
                             as.double(fort[[9]]),
                             as.double(fort[[10]]),
                             as.double(rep(0,length(ds))),
                             PACKAGE = "hydrostreamer")
            setTxtProgressBar(pb, prog)
        }
        # get the outflow matrix from fort object, and add it to output matrix
        fort <- matrix(fort[[10]], 
                       nrow=nrow(outmat), 
                       ncol=ncol(outmat))
        # return NA to the matrix
        fort[inmat_na] <- NA
        outflowmat[,,pred] <- fort
        
        # # process each segment
        # for(seg in order) {
        #     prog <- prog +1
        #     
        #     # get indices of downstream segments
        #     ds <- downstreamind[[seg]]
        #     
        #     # process each timestep in runoff input
        #     for(ts in 1:(tsteps)) {
        #         # inflowing runoff at segment seg, timestep ts
        #         q <- inflowmat[ts,seg,pred]
        #         if(is.na(q)) q <- 0
        #         
        #         # cumulative time through all downstream segments standardized by
        #         # the interval between timesteps
        #         cumulative <- downstream[[seg]]$cumulative / interval[ts]
        #         tf <- floor(cumulative)
        #         tfuni <- unique(tf)
        #         
        #         # Identify last segment water flows through in each segment
        #         last <- c(0, cumsum(rle(tf)$lengths))
        # 
        #         outflow <- matrix(0, 
        #                           nrow = length(tfuni)+1, 
        #                           ncol = length(cumulative))
        #         
        #         for(i in 2:length(last)) {
        #             # first and last segment
        #             f <- last[i-1]+1
        #             l <- last[i]
        #             
        #             # outflow = q * how long it takes to pass through during
        #             # the timestep
        #             outflow[i-1,f:l] <- q*(1-(cumulative[f:l]-tf[f:l]))
        #             
        #             # flow that did not flow through the segments during,
        #             # the timestep, flows through on the next tstep
        #             outflow[i,f:l] <- q-outflow[i-1,f:l]
        #         }
        #         
        #         for(i in 1:nrow(outflow)) {
        #             outflowmat[ts+i-1, ds, pred] <- 
        #                 rowSums(cbind(outflowmat[ts+i-1, ds, pred], 
        #                               outflow[i,]),
        #                         na.rm=TRUE)
        #         } 
        #     }
        #     setTxtProgressBar(pb, prog)
        # }
    }
    
    for(seg in order) {
        out <- flow[[seg]]
        nas <- is.na(out)
        out[,-1] <- outflowmat[(pad_n+1):(tsteps),seg,]
        out[nas] <- NA
        out <- tsibble::as_tsibble(out, index="Date")
        flow[[seg]] <- out
    }
    
    output <- HS 
    output$discharge_ts <- flow
    
    if (verbose) close(pb)
    
    output <- reorder_cols(output)
    output <- assign_class(output, "HS")
    return(output)
}

apply_boundary <- function(control_ts, discharge, type) {
    # check and apply controls condition
    
    dateind <- discharge$Date %in% control_ts$Date
    
    # Set, of modify input runoff of the segment
    if (type == "set") {
        for(pred in 2:ncol(discharge)) {
            discharge[dateind,pred] <- control_ts[,2]
        }
        
        # if no downstream segments, go to next seg
        if(!is.na(nextriver)) {
            new_dis <- discharge[[nextriver[seg] ]][,-1] + 
                discharge[,-1]
            
            discharge[[ nextriver[seg] ]][,-1] <- new_dis
        }
        
    } else if (type == "add") {
        for(pred in 2:ncol(discharge)) {
            discharge[dateind,pred] <- 
                discharge[dateind,pred] + control_ts[,2]
        }
        
    } else if (type == "subtract") {
        for(pred in 2:ncol(discharge)) {
            discharge[dateind,pred] <- 
                discharge[dateind,pred] - control_ts[,2]
        }
        
    } else if (type == "multiply") {
        for(pred in 2:ncol(discharge)) {
            discharge[dateind,pred] <- 
                discharge[dateind,pred] * control_ts[,2]
        }
    }
    return(discharge)
}

