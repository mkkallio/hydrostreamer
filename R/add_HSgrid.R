#' Combine \code{HSgrid} objects or add new runoff input from raster.
#' 
#' Combines the \code{runoff_ts} columns from two HSgrid objects, or adds new
#' runoff timeseries from a raster. **All** geometries in HSgrid must be present
#' in the object to be added, or combination will not done.
#'
#' @param HSgrid an existing \code{HSgrid} object.
#' @param from A \code{HSgrid} object to add from. Optional.
#' @inheritParams raster_to_HSgrid 
#' 
#' @return Returns the input \code{HSgrid} object with added columns in 
#'   runoff_ts column.
#' 
#' @export
add_HSgrid <- function(HSgrid, 
                       from=NULL, 
                       rasters = NULL, 
                       date = NULL, 
                       timestep = NULL, 
                       aoi = NULL, 
                       names=NULL) {
    
    if(!"HSgrid" %in% class(HSgrid)) { 
        stop("HSgrid input should be of class HSgrid")
    }
    
    if (is.null(from)) {
        from <- raster_to_HSgrid(raster, date, timestep, aoi, names)
    }
    
    if(!"HSgrid" %in% class(from)) { 
        stop("'from' input should be of class HSgrid")
    }
    
    # test which elements of from are included in HSgrid
    common_elements <- sf::st_equals(from, HSgrid, sparse = FALSE)
    
    test <- colSums(common_elements)
    if(!all(test == 1)) {
        warning(paste0("Not all cells in HSgrid included in the object to be ",
                    "combined. Returning original with no added timeseries!!"))
        return(HSgrid)
    }
    
    # add timeseries from runoff_ts in 'from' to runoff_ts in 'HSgrid'
    # for all elements in common (same ID)
    for (i in 1:nrow(common_elements)) {
        gid <- which(common_elements[i,])
        if (length(gid) == 0) next
        
        fr <- from$runoff_ts[[i]]
        runoff_names <- colnames(fr)
        
        test <- any(runoff_names[-1] %in% colnames(HSgrid$runoff_ts[[gid]]))
        if(test) warning("Runoff timeseries to be combined contain same
                         names - possible duplicated timeseries!")
        
        new_tsibble <- dplyr::left_join(HSgrid$runoff_ts[[gid]], 
                                        fr,
                                        by="Date")
        HSgrid$runoff_ts[[gid]] <- new_tsibble
    } 
    
    n_ts_new <- lapply(HSgrid$runoff_ts, ncol) %>% 
        unlist()
    
    HSgrid <- HSgrid %>% 
        dplyr::mutate(n_ts = n_ts_new-1)
    
    
    HSgrid <- reorder_cols(HSgrid)
    # earlier mutate changed the class, so assign it again.
    HSgrid <- assign_class(HSgrid, c("HSgrid", "HS"))
    return(HSgrid)
}
