#' @method print HS 
#' @export 
print.HS <- function(x, ...) {
    
    HSgrid <- "HSgrid" %in% class(x)
    
    cat("\nHydrostreamer")
    cat("\n")
    cat(paste0("No. objects: ", nrow(x)))
    cat("\n")
    
    if(hasName(x, "runoff_ts")){
        cat(paste0("No. runoff inputs: ", ncol(x$runoff_ts[[1]]-1)))
        cat("\n")
        cat("  Included runoff timeseries: ")
        print(colnames(x$runoff_ts[[1]][-1]))
        cat("\n")
    }
    
    if(hasName(x, "discharge_ts")){
        cat(paste0("No. discharge timeseries: ", ncol(x$discharge_ts[[1]]-1)))
        cat("\n")
        cat("  Discharge predictions: ")
        print(colnames(x$discharge_ts[[1]][-1]))
        cat("\n")
    }
    
    if(hasName(x, "observation_station")) {
        stations <- unique(x$observation_station)
        stations <- stations[!is.na(stations)]
        
        cat(paste0("No. observation stations: ", length(stations)))
        cat("\n")
        cat("  Stations: ", paste(stations, sep=" "))
        cat("\n")
    }
    
    if(hasName(x, "control_ts")) {
        controls <- table(x$control_type)
        
        cat(paste0("No. of flow controls: ", sum(controls)))
        cat("\n")
        cat("  Control types: ")
        print(controls)
        
    }
    cat("\n")

    NextMethod()
}


#' @method plot HS 
#' @export 
plot.HS <- function(x, ...) {
    
    observation_ts <- NULL
    control_ts <- NULL
    
    test <- hasName(x, "PREVIOUS")
    if(test) x$PREVIOUS <- lapply(x$PREVIOUS, 
                                  function(x) {
                                      paste(x, collapse=" ")
                                  }) %>% unlist()
    
    test <- hasName(x, "runoff_ts") 
    if (test) {
        x$runoff_ts <- rep(TRUE, nrow(x))
    }
    
    test <- hasName(x, "discharge_ts") 
    if (test) {
        x$discharge_ts <- rep(TRUE, nrow(x))
    }
    
    test <- hasName(x, "Optimisation_info") 
    if (test) {
        x$Optimisation_info <- !sapply(x$Optimisation_info, is.null)
    }
    
    test <- hasName(x, "observation_ts") 
    if (test) {
        #x$observation_ts <- !sapply(x$observation_ts, is.null)
        x <- dplyr::select(x, -observation_ts)
    }
    
    test <- hasName(x, "control_ts") 
    if (test) {
        #x$control_ts <- !sapply(x$control_ts, is.null)
        x <- dplyr::select(x, -control_ts)
    } 
    
    NextMethod()
}


#' @export
tsplot <- function(HS, 
                   riverID, 
                   what = "discharge",
                   date_begin = NULL, 
                   date_end = NULL) {
    
    test <- requireNamespace("ggplot2")
    if(!test) stop("ggplot2 not found: Use install.packages('ggplot2') first.")
    
    if(hasName(HS, "observation_ts")) {
        obs <- observations(HS, riverID)[[1]]
    } else obs <- NULL
    
    if(what == "discharge") {
        preds <- discharge(HS, riverID)[[1]] %>%
            tidyr::gather(Pred, Prediction, -Date) 
        what <- "Discharge"
    } else if(what == "runoff") {
        preds <- runoff(HS, riverID)[[1]] %>%
            tidyr::gather(Pred, Prediction, -Date)
        obs <- NULL
        what = "Runoff"
    }
    
    # plot preds
    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data = preds,
                        aes(Date,Prediction, color="1 Prediction", group=Pred), 
                        size=1) 
    
    # plot observations if any
    if(!is.null(obs)) {
        plot <- plot + 
            ggplot2::geom_line(data=obs, aes(Date, observations, 
                               color="2 Station observations"),
                               size=1)
    }

    # modify plot
    plot <- plot +
        ggplot2::scale_color_manual(values = c('grey85','red'),
                                    name = "Timeseries") +
        ggplot2::labs(x="", y="m3/s", 
                      title = paste0(what, " timeseries"),
             subtitle = paste0("at river segment ", riverID)) +
        ggplot2::theme_bw() 
    
    # limit x axis based on dates
    if(!is.null(date_begin)) {
        begin <- as.Date(date_begin)
    } else {
        begin <- as.Date(min(c(obs$Date,preds$Date)))
    } 
    if(!is.null(date_end)) {
        end <- as.Date(date_end)
    } else {
        end <- as.Date(max(c(obs$Date,preds$Date)))
    } 
    plot <- plot + 
        ggplot2::scale_x_date(limits = c(begin, end))
    
    # draw plot
    suppressWarnings(plot)
}

#' Convenience functions to extract timeseries from a HS* object
#' 
#' These functions can be used to quickly extract segment specific timeseries
#' (either discharge, runoff, observations or control) by riverID, or create
#' a table from specified riverIDs with date and timeseries of the river 
#' segments in columns. 
#' 
#' @param HS a \code{HS} object
#' @param riverID A vector of riverIDs for which to extract timeseries. If
#'   \code{NULL} (default), extracts timeseries from all river segments.
#'   
#' @return Returns a list of \code{tsibble}s with column Date and columns 
#'   named by riverID. Each element of the list is named by andcorrespond to a 
#'   timeseries in the timeseries column column specified by function name.
#' 
#' @name Extract_timeseries
NULL

#' @rdname Extract_timeseries
#' @export
discharge <- function(HS, riverID = NULL) {

    if(!is.null(riverID)) {
        test <- all(riverID %in% HS$riverID)
        if(!test) {
            test <- which(!riverID %in% HS$riverID)
            stop(paste0("The following riverID's not found in input HS: ",
                        riverID[test]))
        }
    }
    out <- get_ts(HS, riverID, what="discharge")
    return(out)
}

#' @rdname Extract_timeseries
#' @export
runoff <- function(HS, riverID = NULL) {
    
    if(!is.null(riverID)) {
        test <- all(riverID %in% HS$riverID)
        if(!test) {
            test <- which(!riverID %in% HS$riverID)
            stop(paste0("The following riverID's not found in input HS: ",
                        riverID[test]))
        }
    }
    out <- get_ts(HS, riverID, what="runoff")
    return(out)
}

#' @rdname Extract_timeseries
#' @export
observations <- function(HS, riverID = NULL) {
    
    if(!is.null(riverID)) {
        test <- all(riverID %in% HS$riverID)
        if(!test) {
            test <- which(!riverID %in% HS$riverID)
            stop(paste0("The following riverID's not found in input HS: ",
                        riverID[test]))
        }
    }
    out <- get_ts(HS, riverID, what="observation")
    return(out)
}

#' @rdname Extract_timeseries
#' @export
control <- function(HS, riverID = NULL) {
    
    
    if(!is.null(riverID)) {
        test <- all(riverID %in% HS$riverID)
        if(!test) {
            test <- which(!riverID %in% HS$riverID)
            stop(paste0("The following riverID's not found in input HS: ",
                        riverID[test]))
        }
    }
    out <- get_ts(HS, riverID, what="control")
    return(out)
}

get_ts <- function(HS, riverID = NULL, what) {
    
    what <- paste0(what, "_ts")
    
    if(is.null(riverID)) {
        out <- hydrostreamer:::collect_listc(dplyr::pull(HS, what), acc=TRUE)
        return(out)
    } else if(length(riverID) == 1) {
        ind <- which(HS$riverID %in% riverID)
        out <- dplyr::pull(HS[ind,], what)
        return(out)
    } else {
        ind <- which(HS$riverID %in% riverID)
        out <- hydrostreamer:::collect_listc(dplyr::pull(HS[ind,], what), 
                                             acc=TRUE)
        return(out)
    }
}


#### easily add timeseries
`+.HS` <- function(e1,e2) {
    
}
