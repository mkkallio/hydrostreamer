#' Combine \code{HSgrid} objects or addnew runoff input from raster.
#' 
#' Combines the runoff lists from two HSgrid objects, or adds a new
#' runoff table from a raster. \emph{Warning: The geometries of the
#' combined grids must be identical.}
#'
#' @param HSgrid an existing \code{HSgrid} object.
#' @param from A \code{HSgrid} object to add from. Optional.
#' @inheritParams raster_to_HSgrid 
#' 
#' @return Returns the input \code{HSgrid} object with added runoff.
#' 
#' @export
add_HSgrid <- function(HSgrid, from=NULL, raster = NULL, date = NULL, timestep = NULL, aoi = NULL, name=NULL) {
    
    if(!"HSgrid" %in% class(HSgrid)) { 
        stop("HSgrid input should be of class HSgrid")
    }
    
    if (is.null(from)) {
        from <- raster_to_HSgrid(raster, date, timestep, aoi, name)
    }
    
    if(!"HSgrid" %in% class(from)) { 
        stop("'from' input should be of class HSgrid")
    }
    
    names <- names(from$runoff)
    
    n <- length(HSgrid$runoff)
    for (i in seq_along(from$runoff)) {
        ind <- n+i
        if(is.null(names)) {
            HSgrid$runoff[[ind]] <- from$runoff[[i]]
        } else {
            HSgrid$runoff[[ names[i] ]] <- from$runoff[[i]]
        }
    }
    return(HSgrid)
}
