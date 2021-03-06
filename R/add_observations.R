#' Adds observation timeseries to a 'HS' object
#' 
#' Adds observation timeseries to a HS object. This is needed in order to 
#' evaluate performance of discharge estimates, or to perform data assimilation
#' combining several estimates.
#' 
#' @param HS An \code{HS} object.
#' @param timeseries a data.frame with observations. Must contain column 
#'   \code{Date}.
#' @param unit The unit of values in \code{timeseries}.
#' @param riverIDs A vector of riverID of the river segments in the column-order
#'   in timeseries.
#' @param station_names a vector of names for the stations in \code{timeseries}.
#'   If not provided, station names are obtained from column names of
#'   \code{timeseries}.
#'  
#' @return Returns the \code{HS} object with added list column 
#'   \code{observation_ts} containing a timeseries, and column 
#'   \code{observation_station} containing the name of observation stations.
#' 
#' @export
add_observations <- function(HS, 
                             timeseries,
                             unit,
                             riverIDs, 
                             station_names = NULL) {
    
    Date <- NULL
    
    if (!any(c("Date", "Month") %in% colnames(timeseries))) {
        stop("Observations do not include column 'Date', or 'Month'.")
    } 
    
    if(!"HS" %in% class(HS)) {
        stop("First input must be of class 'HS'")
    }
    
    # set unit
    timeseries <- dplyr::select(timeseries, Date, dplyr::everything())
    for(i in 2:ncol(timeseries)) {
        timeseries[,i] <- units::as_units(dplyr::pull(timeseries,i), unit)
    }
    
    if(is.null(station_names)) station_names <- colnames(timeseries)[-1]
    
    # NaN -> NA; is.nan has no method for lists -> for-loop
    for(i in 1:ncol(timeseries)) {
        if( tolower(colnames(timeseries)[i])== "date") next
        tmp <- dplyr::pull(timeseries,i) # %>% unlist() %>% unname()
        tmp[is.nan(tmp)] <- NA
        timeseries[,i] <- tmp
    }
    
    listc <- spread_listc(list( observations = timeseries))
    
    if(!hasName(HS, "observation_ts")) {
        observations <- vector("list", nrow(HS))
    } else {
        observations <- HS$observation_ts
    }
    
    if(!hasName(HS, "observation_station")) {
        stats <- vector("character", nrow(HS))
    } else {
        stats <- HS$observation_station
    }
   
    
    for(i in seq_along(riverIDs)) {
        statpos <- which(HS$riverID == riverIDs[[i]])
        if(is.null(length(statpos)) || length(statpos) == 0) {
            message(paste0("riverID ", riverIDs[[i]], " does not exist in 
                           HS - skipping station at ",riverIDs[i]))
            next
        }
        stats[statpos] <- station_names[i]
        observations[[ statpos ]] <- listc[[i]]
    }
    stats[stats == ""] <- NA
    
    
    HS$observation_station <- stats
    HS$observation_ts <- observations
    
    HS <- reorder_cols(HS)
    HS <- assign_class(HS, "HS")
    return(HS)
}


