#' Estimate runoff in river reaches 
#'   
#' Estimates runoff in individual segments of a river network represented as 
#' either \code{sf LINESTRING} or \code{sf POLYGON} objects. Allows use of 
#' simple Area Weighted Interpolation, Area-to-line interpolation, Dasymetric
#' Mapping, Pycnophylactic Interpolation, or combined Pycnophylactic-Dasymetric
#' Interpolation.
#' 
#' The river network can be any of three options: 1) a river network consisting
#' of LINESTRINGs, in which case Area-to-Line interpolation is used. 2) A 
#' network of POLYGONs, in which case Areal Interpolation (AWI, DM, or PP) 
#' is used. 3) both LINESTRING river network, and a POLYGON network describing 
#' the catchments of each individual segment of the LINESTRING network. In this 
#' case, Areal Interpolation is used. The third option is useful for routing 
#' purposes. If only catchments are given, only instantaneous routing can be 
#' used, because river line length cannot be derived from polygon areas. 
#' 
#' The selection of areal interpolation method is determined by the input to the
#' function. If \code{dasymetric} and \code{pycnophylactic} are NULL (default),
#' simple Area Weighted Interpolation or Area-to-Line Interpolation is used. If
#' \code{dasymetric} variable is given, Dasymetric Mapping is used instead. 
#' If \code{pycnophylactic} is given, the function performs Pycnophylactic 
#' Interpolation. And if both \code{dasymetric} and \code{pycnophylactic} are 
#' given, the function first performs Pycnophylactic Interpolation, which is 
#' further refined by the given dasymetric variable. Note that pycnophylactic
#' interpolation is only available for polygon networks. Catchments for 
#' linestring networks can be estimated using \code{\link{river_voronoi}},
#' allowing pycnophylactic interpolation for line networks. 
#'
#' @param HS A 'HS' object, obtained with \code{\link{raster_to_HS}} or 
#'   \code{\link{create_HS}}.
#' @param river An 'sf' linestring or polygon feature representing a river 
#'   network
#' @param basins An 'sf' polygon object with corresponding catchments, if river
#'   input is a linestring. Optional.
#' @param dasymetric Column name in \code{river} or in \code{basins} to be used 
#'   as the ancillary information in dasymetric mapping. If \code{NULL} 
#'   (default), no dasymetric mapping is performed.
#' @param pycnophylactic Column in \code{HS} to be used as basis in 
#'   pycnophylactic interpolation. If \code{NULL}, or if \code{is.null(basins)},
#'   not performed.
#' @param n Number of iterations when using pycnophylactic interpolation. 
#'   Default \code{10}.
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
#' @return Returns a \code{HS} object, containing the following 
#'   elements:
#'   \itemize{
#'     \item \code{riverID}. Unique identifier 
#'     \item \code{runoff_ts}. Interpolated runoff timeseries at each river 
#'       reach.
#' }
#'  
#' @export
interpolate_runoff <- function(HS, 
                               river,
                               basins = NULL,
                               dasymetric = NULL,
                               pycnophylactic = NULL,
                               n = 10,
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
    
    
    if(verbose) {
        msg <- paste0("Starting Interpolation from ", nrow(HS), " source zones ",
                      "to ", nrow(river), " target zones.")
        message(msg)
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
    
    if(verbose) {
        msg <- paste0("Interpolating..")
        message(msg)
    }
    
    out <- downscale_runoff(out, verbose=verbose)
    
    return(out)
    
}
