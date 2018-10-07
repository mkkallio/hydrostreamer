#' Computed goodness-of-fit statistics for discharge predictions
#' against observed streamflow
#' 
#' Computed goodness-of-fit statistics using \code{\link[hydroGOF]{gof}}
#' for all the timeseries in the input \code{HSflow} object. 
#' 
#' @inheritParams optimise_point
#' 
#' @return Returns a list with gof-statistics for all the observations in
#'  every discharge prediction in \code{HSflow}.
#' 
#' @export
flow_gof <- function(HSflow, HSobs) {
    UseMethod("flow_gof")
}


#' @export
flow_gof.list <- function(HSflow, HSobs) {
    riverIDs <- HSobs$riverIDs
    obsdata <- HSobs$Observations
    obsdata <- tibble::add_column(obsdata, Type ="Observations", .before=1)
    
    ndis <- length(HSflow)
    names <- names(HSflow)
    if (is.null(names)) names <- paste0("discharge", 1:ndis)
    stat_names <- colnames(HSobs$Observations)[2:ncol(HSobs$Observations)]
    
    
    out <- list()
    for (dis in 1:ndis) {
        gofs <- list()
        for (i in seq_along(riverIDs)) {
            stat <- which(colnames(HSflow[[1]]) %in% riverIDs[i])
            pred <- HSflow[[dis]][(HSflow[[dis]]$Date %in% obsdata$Date),c(1,stat)]
            observ <- HSobs$Observations[HSobs$Observations$Date %in% pred$Date, c(1,i+1)]
            temp <- t(hydroGOF::gof( unlist(pred[,2]), unlist(observ[,2]), na.rm=TRUE))
            gofs[[i]] <- data.frame(Prediction = names[dis], Station = stat_names[i], temp, stringsAsFactors=FALSE)
        }
        gofs <- do.call("bind_rows", gofs)
        out[[dis]] <- gofs
    }
    out <- do.call("bind_rows", out)
    
    return(out)
}


#' @export
flow_gof.HSflow <- function(HSflow, HSobs) {
    riverIDs <- HSobs$riverIDs
    obsdata <- HSobs$Observations
    obsdata <- tibble::add_column(obsdata, Type ="Observations", .before=1)
    
    ndis <- length(HSflow$discharge)
    names <- names(HSflow$discharge)
    if (is.null(names)) names <- paste0("discharge", 1:ndis)
    stat_names <- colnames(HSobs$Observations)[2:ncol(HSobs$Observations)]
    
    
    out <- list()
    for (dis in 1:ndis) {
        gofs <- list()
        for (i in seq_along(riverIDs)) {
            stat <- which(colnames(HSflow$discharge[[1]]) %in% riverIDs[i])
            pred <- HSflow$discharge[[dis]][(HSflow$discharge[[dis]]$Date %in% obsdata$Date),c(1,stat)]
            observ <- HSobs$Observations[HSobs$Observations$Date %in% pred$Date, c(1,i+1)]
            temp <- t(hydroGOF::gof( unlist(pred[,2]), unlist(observ[,2]), na.rm=TRUE))
            gofs[[i]] <- data.frame(Prediction = names[dis], Station = stat_names[i], temp, stringsAsFactors=FALSE)
        }
        gofs <- do.call(dplyr::bind_rows, gofs)
        out[[dis]] <- gofs
    }
    out <- do.call("bind_rows", out)
    
    return(out)
}
