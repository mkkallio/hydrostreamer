#' Apply Muskingum-Cunge river routing
#' 
#' The function implements Muskingum routing scheme where the storage parameter
#' \emph{k} is computed using user input wave celerity, and the length of a 
#' river segment. Using Muskingum for runoff data with time interval day may 
#' cause instability in the output. If the interval is too high, it is advised 
#' to use another routing algorithm.
#' 
#' Muskingum hydrologic routing method consists of two parameters; K and x. K
#' corresponds to travel time through a river segments, and can be estimated 
#' using Wave celerity and length of the flow line. x is an empirical parameter
#' for which the value can be found through trial and error. hydrostreamer 
#' currently does not offer automated calibration of x.
#' 
#' Channel widths can be given as a vector, however by default channel width
#' is computed from a power-law relationship from reference discahrge (q_ref). 
#' If the  \code{length(channel_width) == 2}, the first parameter is alfa and
#' the second is beta in \code{alfa*q_ref^beta}. "Allen" and "Moody" correspond
#' to optimised parameters. 
#' 
#' @param HS A \code{HS} object.
#' @param manning Manning's roughness coefficient. Either a constant, a 
#'   vector of values corresponding to each river segment, or the name of 
#'   the column of \code{HS} where the values are stored.
#' @param slope River bed slopes. Either a constant, a 
#'   vector of values corresponding to each river segment, or the name of 
#'   the column of \code{HS} where the values are stored.
#' @param channel_width Channel widths. See details.
#' @param q_ref Reference discharge for which channel width computations are
#'   based on. The default is \code{NULL}, which means that q_ref is automatically
#'   computed from inflow timeseries.
#' @param celerity If desired, (constant) wave celerities for each river 
#'   segment can be given. Default \code{NULL} means that wave celerity is
#'   automatically computed for each river segment and each timestep.
#' @inheritParams accumulate_runoff
#' 
#' @return Returns the input object \code{HS}) with an added list column
#'   \code{discharge_ts} containing routed discharge estimates for each river
#'    segment. 
#' 
#' @export
accumulate_runoff_muskingum_cunge <- function(HS, 
                                              manning,
                                              slope,
                                              channel_width = "Moody",
                                              q_ref = NULL,
                                              celerity = NULL,
                                              verbose=FALSE) {
    
    riverID <- NULL
    UP_SEGMENTS <- NULL
    Date <- NULL
    fl3h <- NULL
    
    
    # --------------------------------------------------------------------------
    # test inputs and prepare
    
    test <- inherits(HS, "HS")
    if(!test) stop("input HS must be of class HS")
    
    
    if(verbose) message("Getting ready..")
    
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
    
    
    # reference discharge for width calculation
    test <- is.null(q_ref)
    if(test) {
        compute_qref <- TRUE
    } else {
        compute_qref <- FALSE
        test2 <- is.character(q_ref)
        if(test2) {
            test3 <- hasName(HS, q_ref)
            if(test3) {
                q_ref <- dplyr::pull(HS, q_ref)
            }
        }
    }
    
    
    # channel width
    test <- is.numeric(channel_width)
    if(test) {
        test2 <- length(channel_width == 2)
        if(test2) {
            # width <- channel_width[1]*q_ref^channel_width[2]
            compute_width <- TRUE
        } else {
            width <- channel_width
            compute_width <- FALSE
        }
    } else {
        test2 <- is.character(channel_width)
        if(test2) {
            test3 <- hasName(HS, channel_width)
            if(test3) {
                width <- dplyr::pull(HS, channel_width)
                compute_width <- FALSE
            } else {
                compute_width <- TRUE
                width <- NULL
                if(channel_width == "Moody") {
                    channel_width <- c(7.2, 0.50)
                    # width <- 7.2*q_ref^0.50
                } else if(channel_width == "Allen") {
                    channel_width <- c(2.71, 0.557)
                    # width <- 2.71*q_ref^0.557
                } else {
                    stop("Couldn't find parameters for channel_width")
                }
            }
        }
    }
    
    # slopes
    test <- length(slope) %in% c(1, nrow(HS))
    if(!test) stop("Length of slope should be either 1 (constant for all ",
                   "segments), or nrow(HS).")
    
    test <- is.character(slope)
    if(test) {
        test2 <- hasName(HS, slope)
        if(test2) {
            slope <- dplyr::pull(HS, slope)
        } else {
            stop("Couldn't find slope in HS.")
        }
    } else {
        if(length(slope) == 1) slope <- rep(slope, nrow(HS))   
    }
    
    # manning roughness
    test <- length(manning) %in% c(1, nrow(HS))
    if(!test) stop("Length of manning should be either 1 (constant for all ",
                   "segments), or nrow(HS).")
    
    test <- is.character(manning)
    if(test) {
        test2 <- hasName(HS, manning)
        if(test2) {
            manning <- dplyr::pull(HS, manning)
        } else {
            stop("Couldn't find Manning's roughness coefficient in HS.")
        }
    } else {
        if(length(manning) == 1) manning <- rep(manning, nrow(HS))   
    }
    
    
    # celerity
    test <- is.null(celerity)
    if(test) {
        compute_celerity <- TRUE
    } else {
        compute_celerity <- FALSE
        
        test <- length(celerity) %in% c(1, nrow(HS))
        if(!test) stop("Length of celerity should be either 1 (constant for all ",
                       "segments, or nrow(HS).")
        
        test <- is.character(celerity)
        if(test) {
            test2 <- hasName(HS, celerity)
            if(test2) {
                celerity <- dplyr::pull(HS, celerity)
            } else {
                stop("Couldn't find wave celerity in HS.")
            }
        } else {
            if(length(celerity) == 1) celerity <- rep(celerity, nrow(HS))   
        }
    }
    
    
    discharge <- HS$runoff_ts
    runoff <- HS$runoff_ts
    
    dates <- lapply(discharge, function(x) x$Date) %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        lubridate::as_date()
    
    # datetimes <- lubridate::as_datetime(dates)
    
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
    
    dates_long <- ceiling(intervals / 3600)
    dates_long <- rep(dates, times = dates_long)
    
    # # process control timeseries?
    # test <- hasName(HS, "control_ts")
    # if(test) {
    #     boundary_runoff <- unname(which(sapply(HS$control_type, function(x) {
    #         if(is.null(x)) {
    #             return(FALSE)
    #         } else {
    #             return(x[2] == "runoff")
    #         }
    #     })))
    #     if(length(boundary_runoff) == 0) {
    #         rboundary <- FALSE
    #     } else rboundary <- TRUE
    #     boundary_discharge <- unname(which(sapply(HS$control_type, function(x) {
    #         if(is.null(x)) {
    #             return(FALSE)
    #         } else {
    #             return(x[2] == "discharge")
    #         }
    #     })))
    #     if(length(boundary_discharge) == 0) {
    #         dboundary <- FALSE
    #     } else dboundary <- TRUE
    # } else {
    #     rboundary <- FALSE
    #     dboundary <- FALSE
    # }
    # 
    # if(rboundary) {
    #     
    # }
    
    
    
    
    # --------------------------------------------------------------------------
    # process all of downscaled runoff
    total <- length(order)*ncol(discharge[[1]][,-1])
    if (verbose) {
        message("Routing..")
        pb <- txtProgressBar(min = 0, max = total, style = 3)
    }
    prog <- 0
    
    # process every timeseries, every segment in order
    for (pred in 2:ncol(discharge[[1]])){
        
        tempflow <- vector("list", length(order))
        
        for (seg in order) {
            # progress ind
            prog <- prog + 1
            
            # dis <- discharge[[seg]]
            run <- runoff[[seg]]
            
            # for (pred in 2:ncol(discharge[[seg]])){
            
            tempflowtest <- is.null(tempflow[[seg]])
            
            if(tempflowtest) {
                
                inflow <- unlist(run[,pred])
                na <- is.na(inflow)
                inflow <- inflow[!na]
                n_int <- ceiling(intervals[!na] / 3600)
                
                inflow <- rep(inflow, times = n_int)
                inflow <- c(inflow[1], inflow)
                outflow <- vector("numeric", length(inflow))
                outflow[1] <- inflow[1]
            } else {
                inflow <- tempflow[[seg]] %>% 
                    dplyr::full_join(dplyr::select(run, 
                                                   c(1, dplyr::all_of(pred))),
                                     by = "Date") 
                inflow <- rowSums(inflow[,-1])
                na <- is.na(inflow)
                inflow <- inflow[!na]
                
                inflow <- c(inflow[1], inflow)
                outflow <- vector("numeric", length(inflow))
                outflow[1] <- inflow[1]
                
            }
            
            # routing parameters
            par <- MC_parameters(inflow,
                                 seg,
                                 compute_qref,
                                 q_ref,
                                 compute_width,
                                 channel_width,
                                 width,
                                 manning,
                                 slope,
                                 compute_celerity,
                                 celerity)
            
            Cel_vec <- c(par$Cel[1], par$Cel)
            
            # apply routing
            outflow <- do_musk(inflow, outflow, Cel_vec, par$w,
                                               slope[seg], lengths[seg])[-1]
            
            
            # add inflow to next segment
            if(tempflowtest) {
                outflow <- dplyr::tibble(Date = rep(dates[!na], 
                                                    times = n_int),
                                         fl3h = outflow) 
            } else {
                outflow <- dplyr::tibble(Date = dates_long[!na],
                                         fl3h = outflow) 
            }
            
            
            test <- !is.na(nextriver[seg])
            if(test) {
                test <- is.null(tempflow[[ nextriver[seg] ]])
                if(test) {
                    tempflow[[ nextriver[seg] ]] <- outflow
                } else {
                    tempflow[[ nextriver[seg] ]]$fl3h <- 
                        tempflow[[ nextriver[seg] ]]$fl3h + 
                        outflow$fl3h
                }   
            }
            
            # set units and set discharge of current segment
            outflow <- outflow %>% 
                dplyr::group_by(Date) %>% 
                dplyr::summarise(fl3h = mean(fl3h)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(fl3h = units::set_units(fl3h, "m3/s"))
            ind <-  discharge[[ seg ]]$Date %in% outflow$Date
            discharge[[ seg ]][ind, pred] <- outflow$fl3h
            
            # remove used input timeseries to lower memory use
            tempflow[[seg]] <- NA   
            
            #update progressbar
            if (verbose) setTxtProgressBar(pb, prog)
        }
        

        
        
    }
    
    
    # --------------------------------------------------------------------------
    # prepare output
    
    output <- HS 
    output$discharge_ts <- discharge
    output <- output %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        sf::st_as_sf()
    
    if (verbose) close(pb)
    
    output <- reorder_cols(output)
    output <- assign_class(output, "HS")
    return(output)
}
