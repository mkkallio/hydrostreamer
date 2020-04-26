#' Estimate runoff in target features
#'   
#' Estimates runoff in either 
#'
#' @param HS A 'HS' object, obtained with \code{\link{raster_to_HS}}.
#' @param river An 'sf' linestring feature representing a river network.
#' @param basins An 'sf' polygon object.
#' @param dasymetric Column name in \code{river} or in \code{basins} to be used 
#'   as the ancillary information in dasymetric mapping. If \code{NULL} 
#'   (default), no dasymetric mapping is performed. See details.
#' @param pycnophylactic Column in \code{HS} to be used as basis in 
#'   pycnophylactic interpolation. If \code{NULL}, or if \code{is.null(basins)},
#'   not performed. See details.
#' @param n Number of iterations when using pycnophylactic interpolation. 
#'   Default \code{25}.
#' @param intensive Whether the pycnophylactic variable is intensive (density,
#'   like runoff in mm), or not (in which case it is extensive, or counts like
#'   runoff in volume). 
#' @param weights Name of a column in \code{river} to be used directly as 
#'   weights. Defaults to \code{NULL}. See Details.
#' @param aoi An area of interest ('sf' polygon object) used to intersect 
#'   \code{river}. Ignored, if basins provided (in which case, aoi is the union
#'   of \code{basins}). Defaults to \code{NULL}.
#' @param riverID A character string which specifies the name of the column in 
#'   \code{river} containing unique river network identifiers. Defaults to 
#'   \code{"riverID"}.
#' @param verbose Whether or not print progress indicators.
#'   
#'
#' @return Returns a list object with class 'HSweights', containing the following 
#'   elements:
#'   \itemize{
#'     \item \code{river}. The supplied river network with routing information. 
#'       See \code{\link{river_network}} for details.
#'     \item \code{weights}. River network lines or catchment polygon network 
#'       which was used as the basis of weighting. See 
#'       \code{\link{compute_area_weights}} or \code{\link{compute_river_weights}} 
#'       for details.
#'     \item \code{HS}. The input HS object containing runoff information. 
#' }
#'  
#' @export
interpolate_runoff <- function(HS, 
                               river,
                               basins = NULL,
                               dasymetric = NULL,
                               pycnophylactic = NULL,
                               n = 20,
                               intensive = TRUE,
                               weights = NULL, 
                               aoi=NULL, 
                               riverID = "riverID", 
                               verbose=FALSE) {
    
    ##############
    # CHECK INPUTS
    ##############
    if(!"HS" %in% class(HS)) { 
        stop("HS input should be of class HS, obtained with function 
             raster_to_HS() or create_HS()")
    }
    
    
    
    out <- compute_HSweights(HS, 
                             river,
                             basins = basins,
                             dasymetric = dasymetric,
                             pycnophylactic = pycnophylactic,
                             n = n,
                             intensive = intensive,
                             weights = weights, 
                             aoi=aoi, 
                             riverID = riverID, 
                             verbose=verbose)
    out <- downscale_runoff(out, verbose=verbose)
    
    return(out)
    
}
