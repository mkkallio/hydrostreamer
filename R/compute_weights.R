#' Computes weights \code{HSweights} from \code{HS} and a river network
#'   
#' The function computes weights either based on catchment areas (polygons), 
#' or line segments which intersect a polygon with runoff information. Weights 
#' are assigned so that the sum of weights for area or line features within a 
#' polygon equal to 1.
#' 
#' The river network needs to be a "clean", connected network. This means that 
#' connected river segments share a node at the point of confluence, and all 
#' linestrings are cut at segment intersections.
#'
#' Weights should be one of the following: "equal", "length", "strahler", "area", 
#' or a numeric vector which specifies the weight for each river segment. 
#' \itemize{
#'   \item \code{equal} option assigns equal weights to all river segments within 
#'   a polygon.
#'   \item \code{length} option weights river segments within a polygon based on 
#'   the length of the segment.
#'   \item \code{strahler} option weights river segments based on the Strahler 
#'   number computed for the 
#'   supplied river network.
#'   \item A numeric vector with length equal to the number of features in
#'    \code{river}. Weights will be computed within a polygon: \eqn{x_i/sum(x)}
#'   \item \code{area} option weights the river segments based on segment-specific 
#'   catchment area falling inside a polygon. See more details below.
#' }
#' 
#' If line-based weights (equal, length, strahler, user specified vector) are 
#' used, the river network is split at grid cell boundaries before determining 
#' polygon-segment relationship.
#'
#' "area" weights can be used when catchment-based weighting is desired. If no 
#' further data is supplied, the function computes a Voronoi diagram from the 
#' river network (see \code{\link{river_voronoi}} for details). If the basins 
#' are known, they can be supplied which allows skipping the delineation step 
#' entirely.
#'
#'
#' @param HS  A 'HS' object, obtained with \code{\link{raster_to_HS}}.
#' @param river An 'sf' linestring feature representing a river network.
#' @param basins An 'sf' polygon object. If weights are set to "area", providing 
#'   basins skips the delineation process. ID column must have the name as 
#'   \code{riverID} See details. Optional. Defaults to \code{NULL}.
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
compute_HSweights <- function(HS, 
                              river,
                              basins = NULL,
                              dasymetric = NULL,
                              pycnophylactic = NULL,
                              n = 20,
                              intensive = TRUE,
                              weights = NULL, 
                              aoi=NULL, 
                              riverID = "riverID", 
                              verbose = FALSE) {

    ##############
    # CHECK INPUTS
    ##############
    test <- inherits(HS, "HS")
    if(!test) { 
        stop("HS input should be of class 'HS', obtained with function 
             raster_to_HS() or create_HS()")
    }
    test <- inherits(river, "sf")
    if(!test) {
        stop("river input should be of class 'sf'")
    }
    
    ### determine what to do
    test <- is.null(basins) 
    if(test) {
        type <- sf::st_geometry_type(river, by_geometry = FALSE)
        if(type %in% c("LINESTRING", "MULTILINESTRING")) {
            track <- "line"
            if(verbose) message(paste0("No basins provided - downscaling using ",
                                       "river lines"))
        } else if(type %in% c("POLYGON", "MULTIPOLYGON")) {
            track <- "area"
            if(verbose) message(paste0("Basins provided - interpolating ",
                                       "area -> area"))
            basins <- river
        } else {
            stop("Geometry of input 'river' is not LINESTRING, MULTILINESTRING",
                 " POLYGON or MULTIPOLYGON. Consider using function ",
                 "sf::st_collection_extract() to extract the desired type.")
        }
        
        # track <- "line" 
        # if(verbose) message(paste0("No basins provided - downscaling using ",
        #                "river lines"))
    } else {
        track <- "area"
        if(verbose) message(paste0("Basins provided - interpolating ",
                                   "area -> area"))
    }
    
    
    
    #############
    # AREA TRACK
    #############
    
    if (track == "area") {

        # check column names
        test <- hasName(river, riverID) && hasName(basins, riverID)
        if(!test) stop("Both river and basins input must have identically ",
                       "named column",riverID,"specified by parameter riverID")

        test <- riverID == "riverID"
        if(!test) {
            ind <- which(names(river) == riverID)
            river <- tibble::add_column(river, 
                                        riverID = dplyr::pull(river, ind), 
                                        .before=1)
            
            ind <- which(names(basins) == riverID)
            basins <- tibble::add_column(basins, 
                                         riverID = dplyr::pull(basins, ind), 
                                         .before=1)
        }
        
        if(!is.null(weights)) {
            test <- hasName(basins, weights)
            if(!test) stop("No column ", weights, " found in basins input.")
        }
        
        
        # 1. Intersect river network with union of basins, and intersect HS
        # with the union of basins - this is to make sure that the runoff 
        # units is consistent with the basins. If they are not, weights
        # will not equal to 1 for every runoff unit.
        select <- sf::st_intersects(river, 
                                    sf::st_geometry(sf::st_union(basins)), 
                                    sparse=FALSE)
        river <- river[select,]


        # 2. compute basin weights
        if(!is.null(weights)) {
            basins <- compute_area_weights(basins, 
                                           HS, 
                                           pycno = NULL,
                                           dasy = NULL,
                                           weights = weights)
        } else if(!is.null(pycnophylactic)) {
            basins <- compute_area_weights(basins, 
                                           HS, 
                                           pycno = pycnophylactic,
                                           dasy = dasymetric,
                                           intensive = intensive,
                                           weights = NULL)
        } else if(!is.null(dasymetric)) {
            basins <- compute_area_weights(basins, 
                                           HS, 
                                           pycno = NULL,
                                           dasy = dasymetric,
                                           weights = NULL)
        } else {
            basins <- compute_area_weights(basins, 
                                           HS, 
                                           pycno = NULL,
                                           dasy = NULL,
                                           weights = NULL)
        }
        

        # create output 
        HSweights <- create_HSweights(target = river, 
                                      weights = basins, 
                                      source = HS)
    }

    ############
    # LINE TRACK
    ############
    
    if (track == "line") {
        
        # check column names
        test <- riverID == "riverID"
        if(!test) {
            ind <- which(names(river) == riverID)
            river <- tibble::add_column(river, 
                                        riverID = dplyr::pull(river, ind), 
                                        .before=1)
        }
        
        if(!is.null(weights)){
            test <- hasName(river, weights)
            if(!test) stop("No column ", weights, " found in river input.")
        }
        
        
        # 1. crop river network to aoi if given, else intersect it with grid
        if(!is.null(aoi)) {
            select <- sf::st_intersects(river, 
                                        sf::st_geometry(sf::st_union(aoi)), 
                                        sparse=FALSE)
            river <- river[select,]
        } else {
            select <- sf::st_intersects(river, 
                                        sf::st_geometry(sf::st_union(HS)), 
                                        sparse=FALSE)
            river <- river[select,]
        }


        # 2. compute weights based on river lines
        
        if(!is.null(dasymetric)) {
            splitriver <- compute_river_weights(river, 
                                                HS, 
                                                seg_weights = dasymetric,
                                                split=TRUE)
        } else {
            splitriver <- compute_river_weights(river, 
                                                HS, 
                                                seg_weights = NULL,
                                                split=TRUE)
        }

        # create output
        HSweights <- create_HSweights(target = river, 
                                      weights = splitriver, 
                                      source = HS)
    }
    
    return(HSweights)
}
