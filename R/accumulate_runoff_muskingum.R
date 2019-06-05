#' Apply Muskingum river routing
#' 
#' The function implements Muskingum routing scheme where the storage parameter
#' \emph{k} is computed using user input flow velocity, and the length of a 
#' river segment. Using Muskingum for runoff data with time interval day may 
#' cause instability in the output. If the interval is too high, it is advised 
#' to use another routing algorithm.
#' 
#' Warning: The function is experimental and has not been thoroughly tested yet.
#' 
#' @param HS A \code{HS} object.
#' @param velocity Flow velocity applied to compute parameter x. Can be a 
#'   constant, or a vector of flow velocity at each unique river segments.
#'   Flow velocity defaults at 1.
#' @param x Value for parameter x.
#' @inheritParams accumulate_runoff
#' 
#' @return Returns the input object \code{HS}) with an added list column
#'   \code{discharge_ts} containing routed discharge estimates for each river
#'    segment. 
#' 
#' @export
accumulate_runoff_muskingum <- function(HS, 
                                        velocity = 1,
                                        x,
                                        verbose=FALSE) {
    
    riverID <- NULL
    UP_SEGMENTS <- NULL
    
    lengths <- sf::st_length(HS) %>% unclass()
    IDs <- dplyr::select(HS, riverID) %>% 
        sf::st_set_geometry(NULL) %>% 
        unlist()
    order <- HS %>%
        dplyr::select(riverID, UP_SEGMENTS) %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::arrange(UP_SEGMENTS) %>%
        dplyr::select(riverID) %>%
        unlist() %>%
        match(IDs)
    
    nextriver <- HS$NEXT %>%
        match(IDs)
    
    k <- lengths/velocity
    
    # downscaled <- suppressMessages(collect_listc(HS$runoff_ts, acc=TRUE) )
    # discharge <- downscaled
    discharge <- HS$runoff_ts
    
    dates <- lapply(discharge, function(x) x$Date) %>%
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
    
    # krule check
    krule <- mean(intervals) < 2*k*x

    if (any(krule == FALSE)) {
        warning(paste0( (sum(krule == FALSE) / length(krule))*100, 
            " % of river segments do not satisfy
                'dt < 2kx' rule. Results may be unstable." ))
    }
    
    # process all of downscaled runoff
    total <- length(order)
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    prog <- 0
    for (seg in order) {
        # progress ind
        prog <- prog + 1
        
        # check and apply controls condition
        if (hasName(HS, "control_ts")) {
            if(!is.null(HS$control_ts[[seg]])) {
                control_ts <- HS$control_ts[[seg]]
                type <- HS$control_type[[seg]]
                dateind <- discharge[[seg]]$Date %in% control_ts$Date
                
                
                # Set, of modify input runoff of the segment
                if (type == "set") {
                    for(pred in 2:ncol(discharge[[seg]])) {
                        discharge[[seg]][dateind,pred] <- control_ts[,2]
                    }
                    
                    # if no downstream segments, go to next seg
                    if(!is.na(nextriver[[seg]])) {
                        new_dis <- discharge[[nextriver[seg] ]][,-1] + 
                            discharge[[seg]][,-1]
                        
                        discharge[[ nextriver[seg] ]][,-1] <- new_dis
                    }
                    next
                } else if (type == "add") {
                    for(pred in 2:ncol(discharge[[seg]])) {
                        discharge[[seg]][dateind,pred] <- 
                            discharge[[seg]][dateind,pred] + control_ts[,2]
                    }
                } else if (type == "subtract") {
                    for(pred in 2:ncol(discharge[[seg]])) {
                        discharge[[seg]][dateind,pred] <- 
                            discharge[[seg]][dateind,pred] - control_ts[,2]
                    }
                } else if (type == "multiply") {
                    for(pred in 2:ncol(discharge[[seg]])) {
                        discharge[[seg]][dateind,pred] <- 
                            discharge[[seg]][dateind,pred] * control_ts[,2]
                    }
                    
                }
                
            }
        }
        
        
        
        
        # if there is no downstream segments, go to next seg
        if(is.na(nextriver[[seg]])) {
            next
        }
        
        # process all timeseries (cols in discharge[[seg]])
        for (pred in 2:ncol(discharge[[seg]])){
            dis <- discharge[[seg]]
            
            # init matrix
            mat <- matrix(0, nrow(dis)+1, 2)
            mat[2:nrow(mat), 1] <- dis[,pred] %>% 
                unlist()
            mat[1,] <- mat[2,]
            
            
            
            # for all timesteps, do muskingum
            for (t in 1:(nrow(mat)-1)) {
                C0 <- -(k[seg]*x - 0.5*intervals[t]) /
                    (k[seg] - k[seg]*x + 0.5*intervals[t])
                C1 <- (k[seg]*x + 0.5*intervals[t]) /
                    (k[seg] - k[seg]*x + 0.5*intervals[t])
                C2 <- (k[seg] - k[seg]*x - 0.5*intervals[t]) /
                    (k[seg] - k[seg]*x + 0.5*intervals[t])
                
                new_val <- sum(C0 * mat[t+1, 1],
                    C1 * mat[t, 1],
                    C2 * mat[t, 2],
                    na.rm = TRUE)

                mat[t+1, 2] <- new_val
            }
            
            # set negative flows to 0
            mat[mat[, 2] < 0, 2] <- 0
            mat[is.na(mat[,1]),2] <- NA
            
            # update inflow of the next river segment
            discharge[[ nextriver[seg] ]][, pred] <- 
                discharge[[ nextriver[seg] ]][, pred] + mat[2:(nrow(mat)),2]
            
        }
        
        #update progressbar
        if (verbose) setTxtProgressBar(pb, prog)
        
        
    }
    
    output <- HS 
    output$discharge_ts <- discharge
    output <- output %>%
        tibble::as_tibble() %>%
        sf::st_as_sf()
    
    if (verbose) close(pb)
    
    output <- reorder_cols(output)
    output <- assign_class(output, "HS")
    return(output)
}
   