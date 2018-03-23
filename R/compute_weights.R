#' Computes the weights used for each river segment when assigning values of an arbitrarily shaped polygon
#'   network. 
#'   
#'   The function computes the weights either based on catchment areas(polygons), or line segments
#'   which intersect a grid cell. Weights are assigned so that the sum of weights for area or line features
#'   within a grid cell equals to 1.
#'
#' @param river An 'sf' linestring feature representing a river network. Required.
#' @param grid  A 'HSgrid' object. Required.
#' @param weights A character vector specifying type of weights, or a vector of user-specified weights. See
#'   Details section. Required.
#' @param aoi An 'sf' polygon object, or an object which can be converted to 'sf' using st_as_sf(). Optional.
#' @param basins If weights are set to "area", providing basins skips the polygon generation process.
#'   See Details section. Optional.
#' @param drain.dir If weights are set to "area", providing a drainage direction raster enables delineation
#'   of drainage basins from the provided river network. See Details. Optional.
#' @param riverID A character string which specifies the name of the column containing unique identifier in
#'   river network. Defaults to "ID".
#'
#' @section Details:
#'   The river network needs to be connected, so that neighbouring river segments share a node, and all
#'   linestrings are cut at segment intersections (a "clean" network).
#'
#'   Weights should be one of the following: "equal", "length", "strahler", "area", or a numeric vector
#'   which specifies the weight for each river segment. Equal, length, Strahler or user-defined weights are
#'   based on line segments. "equal" assigned grid cell value to all river segments within the cell, equally.
#'   "length" does the same, but weights are based on the length of river segment within the cell, so that
#'   longer segments get higher weights. "strahler" computes the strahler stream order for the river network
#'   and weights are based on that.
#'
#'   If line-based weights are used, the river network is split at grid cell boundaries.
#'
#'   "area" weights is used when catchment-based weighting is desired. It assigns cell value based on the area
#'   of catchment inside the grid cell. If no predefined basins or drainage directions are given, the function
#'   computes Voronoi polygons based on the river network input. If drainage direction raster is given the
#'   function computes catchment areas for each river segment in the input (however, at this stage the algorithm
#'   is implemented in R and is slow).
#'
#'   Area of interest is optional. If provided, the river network will be clipped using it.
#'
#' @return Returns a list object with class 'HSrgrid' or 'HSragrid', depending on the method of weighting.
#'   A 'HSrgrid' object includes the split, routed and weighted river network as first list element, and the
#'   'HSgrid' object  as the second list element. In A 'HSragrid' object, the first list element is the routed
#'   river network, the second element is a weighted area feature (catchment areas split at grid cell boundaries),
#'   and the third list element is the 'HSgrid' object.
#' @export
#'
#'
compute_weights <- function(river, grid, weights, aoi=NULL, basins=NULL, drain.dir=NULL, riverID = "ID") {

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
                stop("no area of interest given. Voronoi polygon based catchments generation requires an area of interest. If application is global, provide shoreline.")
            }
        }

        # IF aoi is given, intersect river network to aoi
        if(!is.null(aoi)) {
            river <- sf::st_intersection(river, sf::st_geometry(aoi))
        }

        # 1. create flow path information
        river <- flow_network(river, ID = riverID)

        # 2. create basins
        if (track == "delineate") {
            outlets <- river_outlets(drain.dir, river)
            basins <- delineate_basin(drain.dir, outlets)
        }
        if (track == "voronoi") {
            message("No predefined basins or drainage direction raster given: creating Voronoi polygons.")
            basins <- river_voronoi(river, aoi)
        }

        # 3. compute basin weights
        basins <- compute_area_weights(basins, grid)

        # create output list and class it
        river_area_grid <- list(river = river, basins = basins, grid = grid)
        class(river_area_grid) <- append(class(river_area_grid),"HSragrid")

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
        river <- split_river_with_grid(river, grid)

        # 2. create flow paths
        river <- flow_network(river)

        # 3. compute weights based on river lines
        river <- compute_river_weights(river, grid, seg_weights = weights)

        # create output list and class it
        river_grid <- list(river = river, grid = grid)
        class(river_grid) <- append(class(river_grid),"HSrgrid")

        #return
        return(river_grid)
    }

}






#' Assign weights to area features within grid cells.
#'
#' @param basins An 'sf' polygon feature.
#' @inheritParams compute_weights
#'
#' @return Returns an 'sf' polygon feature (which is a union of basins, and grid) with an attribute column
#'   'weights', which is the area of the polygon feature inside a grid cell, divided by the total area
#'   of the grid cell.
#' @export
#'
#'
compute_area_weights <- function(basins, grid) {
  keep <- c("ID", "area_m2")

  grid <- grid[, names(grid) %in% keep]
  names(grid)[names(grid)=="ID"] <- "gridID"
  names(grid)[names(grid)=="area_m2"] <- "g_area_m2"

  #produces error. why? --- later
  basins <- suppressMessages(suppressWarnings(sf::st_intersection(basins,grid))) #%>%
      #sf::st_cast("POLYGON")

  #compute areas of each polygon and add it to v. also add unique IDs
  area <- sf::st_area(basins)
  test <- any(names(basins) == "ID")
  if (!test) {
      basins <- tibble::add_column(basins, ID = 1:NROW(basins), b_area_m2 = area, .before = 1)
  }
  test <- any(names(basins) == "b_area_m2")
  if (!test) {
    basins <- tibble::add_column(basins, b_area_m2 = area, .after = "ID")
  }



  # compute weight. unclass to get rid of the m^2 unit that gets carried over from area
  weight <- unclass(basins$b_area_m2)/unclass(basins$g_area_m2) %>%
      unclass()

  if (any(names(basins) == "weights")) {
    message("Replacing existing 'weights' column")
    v <- dplyr::select(basins, -weights)
  }
  basins$weights <- weight
  return(basins)
}






#' Assign weights to area features within grid cells.
#'
#' @param seg_weights A character vector specifying type of weights, or a vector of user-specified
#'   weights. See Details section. Defaults to "length".
#' @inheritParams compute_weights
#'
#' @section Details:
#'   seg_weights should be one of the following: "equal", "length", "strahler", or a numeric vector
#'   which specifies the weight for each river segment. "equal" assigned grid cell value to all river
#'   segments within the cell, equally."length" does the same, but weights are based on the length of
#'   river segment within the cell, so that longer segments get higher weights. "strahler" computes the
#'   strahler stream order for the river network and weights are based on that.
#'
#' @return Returns an 'sf' linestring feature (which is a union of basins, and grid) with an attribute
#'   column 'weights'.
#' @export
#'
#'
compute_river_weights <- function(river, grid, seg_weights = "length") {
  #get elements of rivers intersecting polygons
  riverIntsc <- sf::st_contains(grid,river, sparse=FALSE)

  # if seg_weights is a character vector
  if (is.character(seg_weights)) {
      if(seg_weights == "length") {
        lengths <- sf::st_length(river) %>%
          unclass()
        weight <- apply(riverIntsc,1, compute_segment_weights, lengths)
        weight <- apply(weight,1, FUN=sum)
        weight <- unlist(weight)
        if (any(names(river) == "weights")) {
          message("Replacing existing 'weights' column")
          river <- dplyr::select(river, -weights)
        }
        river$weights  <- weight
        return(river)

      } else if(seg_weights == "equal") {
        #equal weights
        equal <- seq(1,NROW(river))
        weight <- apply(riverIntsc,1, compute_segment_weights, equal)
        weight <- apply(weight,1, FUN=sum)
        weight <- unlist(weight)
        if (any(names(river) == "weights")) {
          message("Replacing existing 'weights' column")
          river <- dplyr::select(river, -weights)
        }
        river$weights  <- weight
        return(river)

      } else if(seg_weights == "strahler") {
        test <- any(names(river) == "STRAHLER")
        if (test) {
          strahler <- river$STRAHLER
        } else {
          river <- river_hierarchy(river)
          strahler <- river$STRAHLER
        }

        weight <- apply(riverIntsc,1, compute_segment_weights, strahler)
        weight <- apply(weight,1, FUN=sum)
        weight <- unlist(weight)
        if (any(names(river) == "weights")) {
          message("Replacing existing 'weights' column")
          river <- dplyr::select(river, -weights)
        }
        river$weights  <- weight
        return(river)

      } else {
        stop("Accepted values for weights are either 'length', 'equal', 'strahler', or a vector of weights. Please check the input.")
      }
  } else if(is.vector(seg_weights)) {

    weight <- apply(riverIntsc,1, compute_segment_weights, seg_weights)
    weight <- apply(weight,1, FUN=sum)
    weight <- unlist(weight)

    if (any(names(river) == "weights")) {
      message("Replacing existing 'weights' column")
      river <- dplyr::select(river, -weights)
    }
    river$weights  <- weight
    return(river)

  } else {
    stop("Accepted values for weights are either 'length', 'equal', 'strahler', or a vector of weights. Please check the input.")
  }
}


#helper function to get the weights
compute_segment_weights <- function(segments, variable) {
  weights <- rep(0, length(segments))
  n <- sum(variable[segments])
  weights[segments] <- variable[segments]/n
  return(weights)

}
