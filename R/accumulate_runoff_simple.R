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
accumulate_runoff_simple <- function(HS, 
                                     velocity = 1,
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
    
    duration <- lengths/velocity
    
    # downscaled <- suppressMessages(collect_listc(HS$runoff_ts, acc=TRUE) )
    # discharge <- downscaled
    discharge <- HS$runoff_ts
    #discharge <- downscaled
    
    # dates and time intervals between dates
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
        dis <- discharge[[seg]]
        dis <- dis[,2:ncol(dis)] #remove date
        dis <- bind_rows(dis[1,], dis)  %>%
            as.matrix()
        
        #remove NAs for fortran
        na <- apply(dis,1, function(x) any(is.na(x)))
        fort <- dis[!na,]
        rows <- nrow(fort)
        cols <- ncol(fort)

        fort <- .Fortran("routesimple",
                        as.integer(nrow(fort)),
                        as.integer(ncol(fort)),
                        as.double(intervals),
                        as.double(duration[seg]),
                        as.double(fort),
                        as.double(fort),
                        PACKAGE = "hydrostreamer")[[6]]
        fort <- matrix(fort, nrow=rows, ncol=cols)
        # update inflow of the next river segment
        dis[!na] <- fort
        ncol <- ncol(discharge[[ nextriver[seg] ]])
        discharge[[ nextriver[seg] ]][, 2:ncol] <-
            discharge[[ nextriver[seg] ]][, 2:ncol] + dis[-1,]

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
