#' Computes weights \code{HSweights} from \code{HSgrid} and a river network
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
#' Area of interest is optional. If provided, the river network will be clipped using 
#' the supplied AoI.
#'
#' @param HSgrid  A 'HSgrid' object, obtained with \code{\link{raster_to_HSgrid}}.
#' @param river An 'sf' linestring feature representing a river network.
#' @param weights A character vector specifying type of weights, or a vector of user-
#'   specified weights. See Details.
#' @param aoi An area of interest. 'sf' polygon object. Optional.
#' @param basins An 'sf' polygon object. If weights are set to "area", providing basins 
#'   skips the delineation process. ID column must have the name as \code{riverID} See 
#'   Details. Optional.
#' @param riverID A character string which specifies the name of the column in 
#'   \code{river} containing unique river network identifiers. Defaults to "riverID".
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
#'     \item \code{HSgrid}. HSgrid object containing runoff information. See 
#'       \code{\link{raster_to_HSgrid}} for details.
#' }
#'  
#' @export
compute_HSweights <- function(HSgrid, 
                              river,
                              weights, 
                              aoi=NULL, 
                              basins=NULL, 
                              riverID = "riverID", 
                              verbose=FALSE) {

    ##############
    # CHECK INPUTS
    ##############
    if(!"HSgrid" %in% class(HSgrid)) { 
        stop("HSgrid input should be of class HSgrid, obtained with function 
             raster_to_HSgrid()")
    }
    
    # Convert river and aoi to sf
    # check if 'sf'
    test <- any(class(river) == 'sf')
    if(!test) river <- sf::st_as_sf(river)
    # if(!is.null(aoi)) {
    #     test <- 'sf' %in% class(aoi)
    #     if(!test) aoi <- sf::st_as_sf(aoi)
    # }
    
    # check the weights input to determine which track to take (area or line)
    if(is.character(weights)) {
        linetracks <- c("equal", "length", "strahler")
        if(weights == "area") {
          track <- "area"
        } else if(weights %in% linetracks)  {
          track <- "line"
        }
    } else if (is.vector(weights)) {
        track <- "line"
        if (!length(weights) == nrow(river)) {
            stop("Length of 'weights' vector needs to equal to number of features 
                 in 'river'")
        }
    } else {
        stop("weights should be one of 'equal', 'length', 'strahler', 'area', or a 
             vector of weights equaling the length of river segments.")
    }

    
    
    #############
    # AREA TRACK
    #############
    
    if (track == "area") {
        if (!is.null(basins)) {
            track <- "user"
        } else {
            track <- "voronoi"
            if(is.null(aoi)) {
                stop("no area of interest given. Voronoi polygon based catchment 
                     generation requires an area of interest. If application is 
                     global, provide shoreline.")
            }
        }

        # IF aoi is given, intersect river network to aoi
        if(!is.null(aoi)) {
            river <- suppressMessages(
                suppressWarnings(
                    sf::st_intersection(river, sf::st_geometry(aoi))
                )
            )
        }

        # 1. create flow path information
        river <- river_network(river, riverID = riverID, verbose = verbose)

        # 2. create basins
        # if (track == "delineate") {
        #     outlets <- river_outlets(river, drain.dir)
        #     basins <- delineate_basin(outlets, drain.dir, verbose = verbose)
        # }
        if (track == "voronoi") {
            message("No predefined basins or drainage direction raster given: 
                    creating a river Voronoi diagram.")
            basins <- river_voronoi(river, aoi, verbose = verbose)
        }

        # 3. compute basin weights
        basins <- compute_area_weights(basins, 
                                       HSgrid, 
                                       riverID = riverID)

        # create output 
        HSweights <- create_HSweights(river, basins, HSgrid)
    }

    ############
    # LINE TRACK
    ############
    
    if (track == "line") {
        # crop river network to aoi
        if(!is.null(aoi)) {
            select <- sf::st_intersects(river, 
                                        sf::st_geometry(sf::st_union(aoi)), 
                                        sparse=FALSE)
            river <- river[select,]
        }

        # 1. create flow paths
        river <- river_network(river, riverID = riverID, verbose = verbose)

        # 2. compute weights based on river lines
        splitriver <- compute_river_weights(river, 
                                            HSgrid, 
                                            seg_weights = weights,
                                            split=TRUE)

        # create output
        HSweights <- create_HSweights(river = river, 
                                      weights = splitriver, 
                                      HSgrid = HSgrid)
        
    }
    
    return(HSweights)
}
