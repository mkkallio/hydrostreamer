#' Creates a \code{HSweights} object
#' 
#' Creates a list object with class \code{HSweights}. The river object
#' can be obtained with \code{\link{river_network}}, weights with
#' \code{\link{compute_river_weights}} or \code{\link{compute_area_weights}},
#' and grid with \code{\link{raster_to_HSgrid}}.
#' 
#' @param river A river network as an \code{sf LINESTGRING} object
#' @param weights Weights with riverID, gridID and weight information.
#' @param grid A \code{HSgrid} object.
#' 
#' @return Returns a \code{HSweights} object, which is a list of three 
#'   components:
#'   \itemize{
#'     \item river: River network with routing information to be used in
#'       downscaling step.
#'     \item weights: Weighting information for the downscaling step.
#'     \item grid: \code{HSgrid} object with runoff timeseries information.
#'   }
#' 
#' @export
create_HSweights <- function(river, weights, grid, riverID = "riverID") {
    
    test <- all(c(riverID,"NEXT", "PREVIOUS") %in% colnames(river))
    if (!test) stop("river input must have columns riverID, NEXT, PREVIOUS")
    
    test <- all(c("riverID", "gridID", "weight") %in% colnames(weights))
    if (!test) stop("river input must have columns riverID, gridID, weight")
    
    test <- class(grid) == "HSgrid"
    if (!test) stop("grid input must be of class HSgrid")
    
    output <- list(river = river,
                   weights = weights,
                   grid = grid)
    class(output) <- c("HSweights", class(output))
    
}
