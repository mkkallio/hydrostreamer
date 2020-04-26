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
    common_elements <- sf::st_equals(from, HS, sparse = FALSE)
    
    test <- colSums(common_elements)
    if(!all(test == 1)) {
        warning(paste0("Not all cells in HS included in the object to be ",
                    "combined. Returning original with no added timeseries!!"))
        return(HS)
    }
    
    # add timeseries from runoff_ts in 'from' to runoff_ts in 'HS'
    # for all elements in common (same ID)
    for (i in 1:nrow(common_elements)) {
        gid <- which(common_elements[i,])
        if (length(gid) == 0) next
        
        fr <- from$runoff_ts[[i]]
        runoff_names <- colnames(fr)
        
        test <- any(runoff_names[-1] %in% colnames(HS$runoff_ts[[gid]]))
        if(test) warning("Runoff timeseries to be combined contain same
                         names - possible duplicated timeseries!")
        
        new_tsibble <- dplyr::left_join(HS$runoff_ts[[gid]], 
                                        fr,
                                        by="Date")
        HS$runoff_ts[[gid]] <- new_tsibble
    } 
    
    n_ts_new <- lapply(HS$runoff_ts, ncol) %>% 
        unlist()
    
    HS <- HS %>% 
        dplyr::mutate(n_ts = n_ts_new-1)
    
    
    HS <- reorder_cols(HS)
    HS <- assign_class(HS, c("HS"))
    return(HS)
}
