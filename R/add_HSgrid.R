#' Combine \code{HS} objects or add new runoff input from raster.
#' 
#' Combines the \code{runoff_ts} columns from two HS objects, or adds new
#' runoff timeseries from a raster. **All** geometries in HS must be present
#' in the object to be added, or combination will not done.
#'
#' @param HS an existing \code{HS} object.
#' @param from A \code{HS} object to add from. Optional.
#' @inheritParams raster_to_HS 
#' 
#' @return Returns the input \code{HS} object with added columns in 
#'   runoff_ts column.
#' 
#' @export
add_HS <- function(HS, 
                   from=NULL, 
                   rasters = NULL, 
                   unit = NULL,
                   date = NULL, 
                   timestep = NULL, 
                   aoi = NULL, 
                   names=NULL) {
    
    if(!"HS" %in% class(HS)) { 
        stop("HS input should be of class HS")
    }
    
    if (is.null(from)) {
        from <- raster_to_HS(rasters, unit, date, timestep, aoi, names)
    }
    
    if(!"HS" %in% class(from)) { 
        stop("'from' input should be of class HS")
    }
    
    # test which elements of from are included in HS
    common_elements <- sf::st_equals(HS, from)
    
    test <- sapply(common_elements, length)
    if(!all(test > 0)) {
        stop("Not all cells in HS included in the object to be ",
             "combined.")
    }
    
    # add timeseries from runoff_ts in 'from' to runoff_ts in 'HS'
    # for all elements in common (same ID)
    for (i in seq_along(common_elements)) {
        gid <- common_elements[[i]]
        if (length(gid) == 0) next
        
        fr <- from$runoff_ts[[gid]]
        runoff_names <- colnames(fr)
        
        test <- any(runoff_names[-1] %in% colnames(HS$runoff_ts[[i]]))
        if(test) warning("Runoff timeseries to be combined contain same
                         names - possible duplicated timeseries!")
        
        new_tibble <- dplyr::left_join(HS$runoff_ts[[i]], 
                                        fr,
                                        by="Date")
        HS$runoff_ts[[i]] <- new_tibble
    } 

    HS <- reorder_cols(HS)
    HS <- assign_class(HS, c("HS"))
    return(HS)
}
