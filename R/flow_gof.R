#' Computed goodness-of-fit statistics for discharge predictions
#' against observed streamflow
#' 
#' Computed goodness-of-fit statistics using \code{\link[hydroGOF]{gof}}
#' for all the timeseries in the input \code{HSflow} object. 
#' 
#' By default, goodness-of-fit measures are computed for all timeseries in 
#' the input data. If weights are provided, the timeseries are
#' first combined according to the weights, and goodness-of-fit are then 
#' computed for the resulting combination. If a list of weights is provided,
#' goodness-of-fit is computed for all of them.
#' 
#' @param weights a numeric vector of weights, or a list of weight vectors 
#'   with the length of the number of timeseries in \code{HSflow}. See details. 
#'   Optional.
#' @inheritParams optimise_point
#' 
#' @return Returns a list with gof-statistics for all the observations in
#'  every discharge prediction in \code{HSflow}.
#' 
#' @export
flow_gof <- function(HSflow, HSobs, weights=NULL) {
    UseMethod("flow_gof")
}


#' @export
flow_gof.list <- function(HSflow, HSobs, weights=NULL) {
    riverIDs <- HSobs$riverIDs
    obsdata <- HSobs$Observations
    obsdata <- tibble::add_column(obsdata, Type ="Observations", .before=1)
    
    colinds <- lapply(HSflow, FUN=function(x) which(colnames(x) %in% riverIDs))
    if(!is.null(weights)) {
        for(i in seq_along(HSflow)) {
            HSflow[[i]] <- HSflow[[i]][,c(1,colinds[[i]])]
        }
        HSflow <- combine_runoff(HSflow, weights)
    }
    
    ndis <- length(HSflow)
    names <- names(HSflow)
    if (is.null(names)) names <- paste0("discharge", 1:ndis)
    stat_names <- colnames(HSobs$Observations)[2:ncol(HSobs$Observations)]
    
    
    out <- list()
    for (dis in seq_along(HSflow)) {
        gofs <- list()
        for (i in seq_along(riverIDs)) {
            stat <- which(colnames(HSflow[[dis]]) %in% riverIDs[i])
            pred <- HSflow[[dis]][(HSflow[[dis]]$Date %in% obsdata$Date),c(1,stat)]
            observ <- HSobs$Observations[(HSobs$Observations$Date %in% pred$Date), c(1,i+1)]
            
            
            if(all(is.na(observ[,2])) || all(is.na(pred[,2]))) {
                gofs[[i]] <- data.frame(Prediction = names[dis],
                                        Station = stat_names[i],
                                        Error = "No common values in pred and obs")
            } else {
                temp <- t(hydroGOF::gof( unlist(pred[,2]), unlist(observ[,2]), na.rm=TRUE))
                gofs[[i]] <- data.frame(Prediction = names[dis], 
                                        Station = stat_names[i], 
                                        temp, 
                                        stringsAsFactors=FALSE)
                
            }
            
        }
        suppressWarnings(gofs <- do.call(dplyr::bind_rows, gofs))
        out[[dis]] <- gofs
    }
    out <- do.call("bind_rows", out)
    
    return(out)
}


#' @export
flow_gof.HSflow <- function(HSflow, HSobs, weights=NULL) {
    out <- flow_gof.list(HSflow$discharge, HSobs, weights)
    return(out)
}
