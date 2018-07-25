#' Computes weights to assign runoff from runoff area features to the supplied river network.
#' 
#'   
#' The function computes weights either based on catchment areas (polygons), or line segments which 
#' intersect a polygon with runoff information. Weights are assigned so that the sum of weights for area or 
#' line features within a polygon equal to 1.
#' 
#' The river network needs to be connected, so that connected river segments share a node, and all
#' linestrings are cut at segment intersections (a "clean" network).
#'
#' Weights should be one of the following: "equal", "length", "strahler", "area", or a numeric vector
#' which specifies the weight for each river segment. 
#' \itemize{
#'   \item \code{equal} option assigns equal weights to all river segments within a polygon.
#'   \item \code{length} option weights river segments within a polygon based on the length of the segment.
#'   \item \code{strahler} option weights river segments based on the Strahler number computed for the 
#'   supplied river network.
#'   \item A numeric vector with length equal to the number of features in \code{river}. Weights will be
#'   computed within a polygon: \eqn{x_i/sum(x)}
#'   \item \code{area} option weights the river segments based on segment-specific catchment area falling
#'   inside a polygon. See more details below.
#' }
#' 
#' If line-based weights (equal, length, strahler, user specified vector) are used, the river network is split 
#' at grid cell boundaries before determining polygon-segment relationship.
#'
#' "area" weights can be used when catchment-based weighting is desired. If no further data is supplied, the 
#' function computes a Voronoi diagram from the river network (see \code{\link{river_voronoi}} for details). By
#' providing a drainage direction raster, segment specific catchment are delineated based on it (see
#' \code{\link{delineate_basin}} for details). If the basins are known, they can be supplied which allows
#' skipping the delineation step entirely.
#'
#' Area of interest is optional. If provided, the river network will be clipped using the supplied AoI.
#'
#' @param river An 'sf' linestring feature representing a river network.
#' @param grid  A 'HSgrid' object, obtained with \code{\link{polygrid_timeseries}}.
#' @param weights A character vector specifying type of weights, or a vector of user-specified weights. See
#'   Details.
#' @param aoi An 'sf' polygon object. Optional.
#' @param basins An 'sf' polygon object. If weights are set to "area", providing basins skips the delineation 
#' process. See Details. Optional.
#' @param drain.dir A RasterLayer object. If weights are set to "area", providing a drainage direction raster 
#' enables delineation of drainage basins for the provided river network. Optional.
#' @param riverID A character string which specifies the name of the column in \code{river} containing unique river network 
#' identifiers. Defaults to "riverID".
#' @param verbose Whether or not print progress indicators.
#'   
#'
#' @return Returns a list object with class 'HSweights', containing the following elements:
#' \itemize{
#'   \item \code{river}. The supplied river network with routing information. See \code{\link{flow_network}} 
#'   for details.
#'   \item \code{weights}. River network lines or catchment polygon network which was used as the basis of 
#'   weighting. See \code{\link{compute_area_weights}} or \code{\link{compute_river_weights}} for details.
#'   \item \code{grid}. HSgrid object containing runoff information. See \code{\link{polygrid_timeseries}} for
#'   details.
#' }
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
#' # weights based on line segments
#' # equal
#' eq <- compute_weights(river, grid, "equal", aoi=basin, riverID="ID")
#' # length
#' len <- compute_weights(river, grid, "length", aoi=basin, riverID="ID")
#' # strahler
#' str <- compute_weights(river, grid, "strahler", aoi=basin, riverID="ID")
#' # vector of weights
#' w <- rnorm(nrow(river))
#' vec <- compute_weights(river, grid, w, aoi=basin, riverID="ID")
#' 
#' # catchment based weights
#' # Voronoi diagram
#' vor <- compute_weights(river, grid, "area", aoi=basin, riverID="ID")
#' # using predefined basins
#' basins <- river_voronoi(river, aoi=basin, riverID = "ID")
#' bas <- compute_weights(river, grid, "area", basins=basins, aoi=basin, riverID="ID")
#' }
#' 
#' @export
compute_weights <- function(river, grid, weights, aoi=NULL, basins=NULL, drain.dir=NULL, riverID = "riverID", verbose=FALSE) {

    if(!any(class(grid) == "HSgrid")) {
        stop("grid input should be of class HSgrid, obtained with function polygrid_timeseries()")
    }
    
    
    # Convert river and aoi to sf
    # check if 'sf'
    test <- any(class(river) == 'sf')
    if(!test) river <- sf::st_as_sf(river)


    if(!is.null(aoi)) {
        test <- any(class(aoi) == 'sf')
        if(!test) aoi <- sf::st_as_sf(aoi)
    }
    
    
    
   

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
            stop("Length of 'weights' vector needs to equal to number of features in 'river'")
        }
    } else {
        stop("weights should be one of 'equal', 'length', 'strahler', 'area', or a vector of weights equaling the length of river segments.")
    }

    # AREA TRACK
    if (track == "area") {
        if(!is.null(drain.dir)) {
            track <- "delineate"
        } else if (!is.null(basins)) {
            track <- "user"
        } else {
            track <- "voronoi"
            if(is.null(aoi)) {
                stop("no area of interest given. Voronoi polygon based catchment generation requires an area of interest. If application is global, provide shoreline.")
            }
        }

        # IF aoi is given, intersect river network to aoi
        if(!is.null(aoi)) {
            river <- suppressMessages(suppressWarnings(sf::st_intersection(river, sf::st_geometry(aoi))))
        }

        # 1. create flow path information
        river <- flow_network(river, riverID = riverID, verbose = verbose)

        # 2. create basins
        if (track == "delineate") {
            outlets <- river_outlets(river, drain.dir)
            basins <- delineate_basin(outlets, drain.dir, verbose = verbose)
        }
        if (track == "voronoi") {
            message("No predefined basins or drainage direction raster given: creating a river Voronoi diagram.")
            basins <- river_voronoi(river, aoi, verbose = verbose)
        }

        # 3. compute basin weights
        basins <- compute_area_weights(basins, grid)

        # create output list and class it
        river_area_grid <- list(river = river, weights = basins, grid = grid)
        class(river_area_grid) <- append(class(river_area_grid),"HSweights")

        #return
        return(river_area_grid)
    }


    # LINE TRACK
    if (track == "line") {
        # crop river network to aoi
        if(!is.null(aoi)) {
            river <- sf::st_intersection(river, sf::st_geometry(aoi))
        }

        # 1. split river to grid
        splitriver <- split_river_with_grid(river, grid, riverID = riverID)

        # 2. create flow paths
        river <- flow_network(river, riverID = riverID, verbose = verbose)

        # 3. compute weights based on river lines
        splitriver <- compute_river_weights(splitriver, grid, seg_weights = weights)

        # create output list and class it
        river_grid <- list(river = river, weights = splitriver, grid = grid)
        class(river_grid) <- append(class(river_grid),"HSweights")

        #return
        return(river_grid)
    }

}
