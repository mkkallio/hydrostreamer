#' Computes specific runoff generated in the river segment.
#' 
#' Computes downscaled, unrouted, river segment specific runoff using the given \code{HSweights} object. 
#'
#' @param HSweights An object of class 'HSweights', obtained with \code{\link{compute_weights}}.
#' @param timesteps A logical vector of the length of timesteps in x, or a numeric vector specifying which 
#' timesteps to process. If no timesteps are given, all will be processed.
#' @param unit Unit of runoff. Can be either "mm/s", or "m3/s". Defaults to mm/s (equivalent to kg/m2/s).
#' @param rID Name of the column in river with unique IDs.
#' @param wID Name of the column in weights with IDs corresponding to rID.
#' @inheritParams compute_weights
#'
#' @return The routed river network object which has been enhanced with a runoff timeseries. Runoff is given
#' in \eqn{m^3/s}.
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(hydrostreamer)
#' 
#' # load data
#' data(river)
#' data(basin)
#' runoff <- brick(system.file("extdata", "runoff.tif", package = "hydrostreamer"))
#' 
#' # create HSgrid
#' grid <- polygrid_timeseries(grid, aoi=basin)
#' 
#' # weights based on length of river segments
#' len <- compute_weights(river, grid, "length", aoi=basin, riverID="ID")
#' 
#' # compute segment specific runoff
#' runoff <- compute_segment_runoff(len, verbose=TRUE)
#' }
#' 
#' @export
compute_segment_runoff <- function(HSweights, rID = "riverID", wID = "riverID", timesteps = NULL, 
                                   unit = "mm/s", verbose = FALSE) {
    
    if(!any(class(HSweights) == "HSweights")) {
        stop("Input should be of class HSweights. Otherwise use function compute_segment_runoff_without_HSweights()")
    }
    
    runoff <- compute_segment_runoff_without_HSweights(HSweights$river, HSweights$weights, HSweights$grid, 
                                                       rID=rID, wID=wID, timesteps=timesteps, unit=unit, verbose=verbose)
    
    return(runoff)
}
