#' Adds flow controls to an 'HS' object
#' 
#' Adds flow control timeseries to a \code{HS} object. Control 
#' conditions should cover the entire timeseries in \code{HS}.
#' 
#' Available control schemes are 
#' \itemize{
#'   \item set: set input flow at a river segment (override routing). Useful 
#'     e.g. for boundary conditions or dams.
#'   \item add: increase runoff at a river segment.
#'   \item subtract: lower runoff at a river segment.
#'   \item multiply: modify runoff.
#' }
#' 
#' @param HS An \code{HS} object.
#' @param timeseries a data frame with observations.
#' @param unit The unit of values in \code{timeseries}. Should be convertible to
#'   cubic meters per second.
#' @param riverIDs A vector of riverID of the river segments of the columns
#'  in obs.
#' @param control Type of control, see details.
#' @param type Whether the control should apply to \code{runoff} input, or to
#'   \code{discharge} output.
#'   
#'  
#' @return Returns the \code{HS} object with added list column 
#'   \code{control_ts} and \code{control_type}.
#' 
#' @export
add_control <- function(HS, timeseries, unit, riverIDs, 
                        control, type) {
    
    Date <- NULL
    
    if (!"Date" %in% colnames(timeseries)) {
        stop("timeseries do not include column 'Date'")
    } 
    
    if(!inherits(HS, "HS")) stop("First input must be of class 'HS'")
    
    # set unit
    timeseries <- dplyr::select(timeseries, Date, dplyr::everything())
    for(i in 2:ncol(timeseries)) {
        ts <- dplyr::pull(timeseries,i)
        if(inherits(ts, "units")) {
            tsunit <- units::deparse_unit(ts)
            if(tsunit != "m3 s-1") ts <- convert_unit(ts, to1 = "m3/s")
            timeseries[,i] <- ts
        } else {
            timeseries[,i] <- units::as_units(dplyr::pull(timeseries,i), unit)   
        }
    }
    
    
    listc <- spread_listc( list( timeseries = timeseries))
    
    if (hasName(HS, "control_ts")) {
        control_ts <- HS$control_ts
    } else {
        control_ts <- vector("list", nrow(HS))
    }
    
    if (hasName(HS, "control_type")) {
        controltype <- HS$control_type
    } else {
        controltype <- vector("list", nrow(HS))
    }
    
    
    
    for(i in seq_along(riverIDs)) {
        statpos <- which(HS$riverID == riverIDs[[i]])
        test <- (is.null(length(statpos)) || length(statpos) == 0)
        if(test) {
            warning(paste0("riverID ", riverIDs[[i]], " does not exist in 
                           HS - skipping station at ",riverIDs[i]))
                           
            next
        }
        control_ts[[statpos]] <- listc[[i]]
        controltype[[statpos]] <- c(operation = control, target = type)
    }
    
    HS$control_ts <- control_ts
    HS$control_type <- controltype
    
    HS <- reorder_cols(HS)
    HS <- assign_class(HS, "HS")
    return(HS)
}

