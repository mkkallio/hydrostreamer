#' Adds flow controls to a 'HS' object
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
#' @param riverIDs A vector of riverID of the river segments of the columns
#'  in obs.
#' @param control Type of control, see details.
#'  
#' @return Returns the \code{HS} object with added list column 
#'   \code{observation_ts}.
#' 
#' @export
add_control <- function(HS, timeseries, riverIDs, control = "set") {
    if (!any(c("Date", "Month") %in% colnames(timeseries))) {
        stop("timeseries do not include column 'Date', or 'Month'.")
    } 
    
    if(!"HS" %in% class(HS)) stop("First input must be of class 'HS'")
    
    listc <- spread_listc( list( timeseries = timeseries))
    
    if (hasName(HS, "control_ts")) {
        control_ts <- HS$control_ts
    } else {
        control_ts <- vector("list", nrow(HS))
    }
    
    if (hasName(HS, "control_type")) {
        controltype <- HS$control_type
    } else {
        controltype <- rep(NA, nrow(HS))
    }
    
    
    
    for(i in seq_along(riverIDs)) {
        statpos <- which(HS$riverID == riverIDs[[i]])
        if(is.null(length(statpos))) {
            message(paste0("riverID ", riverIDs[[i]], " does not exist in 
                           HS - skipping station ", 
                           colnames(control_ts)[-c("Date")]))
            next
        }
        control_ts[[statpos]] <- listc[[i]]
        controltype[[statpos]] <- control
    }
    
    HS$control_ts <- control_ts
    HS$control_type <- controltype
    
    HS <- reorder_cols(HS)
    HS <- assign_class(HS, "HS")
    return(HS)
}

