#' Creates a \code{HSweights} object
#' 
#' Creates a list object with class \code{HSweights}. The river object
#' can be obtained with \code{\link{river_network}}, weights with
#' \code{\link{compute_river_weights}} or \code{\link{compute_area_weights}},
#' and grid with \code{\link{raster_to_HS}}.
#' 
#' @param target A river network as an \code{sf LINESTGRING} object
#' @param weights A data.frame with columns "riverID" in "zoneID", and "weight"
#'   corresponding to riverID in \code{river}, zoneID in \code{HS} and 
#'   a numeric weight.
#' @param source A \code{HS} object.
#' 
#' @return Returns a \code{HSweights} object, which is a list of three 
#'   components:
#'   \itemize{
#'     \item target: River network with routing information to be used in
#'       downscaling step.
#'     \item weights: Weighting information for the downscaling step.
#'     \item source: \code{HS} object with runoff timeseries information.
#'   }
#' 
create_HSweights <- function(target, weights, source) {
    
    test <- all(c("riverID", "zoneID", "weights") %in% colnames(weights))
    if (!test) stop("weights input must have columns riverID, zoneID, weights")
    
    test <- "riverID" %in% colnames(target)
    if (!test) stop("target input must have column riverID, zoneID, weights")
    
    test <- all(c("zoneID", "runoff_ts") %in% colnames(source))
    if (!test) stop("weights input must have columns riverID, zoneID, weights")
    
    output <- list(target = target,
                   weights = weights,
                   source = source)
    output <- assign_class(output, "HSweights")
    return(output)
}
