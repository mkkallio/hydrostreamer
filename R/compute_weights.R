# SF FUNCTIONS

compute_weights <- function(river, grid, weights, aoi=NULL, basins=NULL, drain.dir=NULL, riverID = "ID") {

    # Convert river and aoi to sf
    river <- st_as_sf(river)

    if(!is.null(aoi)) {
        aoi <- st_as_sf(aoi)
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
            river <- st_intersection(river, st_geometry(aoi))
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
        river_area_grid <- list(river, basins, grid)
        class(river_area_grid) <- append(class(river_area_grid),"HSragrid")

        #return
        return(river_area_grid)
    }


    # LINE TRACK
    if (track == "line") {
        # crop river network to aoi
        if(!is.null(aoi)) {
            river <- st_intersection(river, st_geometry(aoi))
        }

        # 1. split river to grid
        river <- split_river_with_grid(river, grid)

        # 2. create flow paths
        river <- flow_network(river)

        # 3. compute weights based on river lines
        river <- compute_river_weights(river, grid, segment.weights = weights)

        # create output list and class it
        river_grid <- list(river, grid)
        class(river_grid) <- append(class(river_grid),"HSrgrid")

        #return
        return(river_grid)
    }

}






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





##
compute_river_weights <- function(river, grid, segment.weights = "length") {
  #get elements of rivers intersecting polygons
  riverIntsc <- sf::st_contains(grid,river, sparse=FALSE)

  # if segment.weights is a character vector
  if (is.character(segment.weights)) {
      if(segment.weights == "length") {
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

      } else if(segment.weights == "equal") {
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

      } else if(segment.weights == "strahler") {
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
  } else if(is.vector(segment.weights)) {

    weight <- apply(riverIntsc,1, compute_segment_weights, segment.weights)
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
