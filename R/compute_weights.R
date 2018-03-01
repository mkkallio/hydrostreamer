# SF FUNCTIONS












compute_area_weights <- function(voronoi, grid) {
  keep <- c("ID", "area_m2")

  grid <- grid[, names(grid) %in% keep]

  #produces error. why? --- later
  v <- suppressMessages(suppressWarnings(st_intersection(voronoi,grid)))
  names(v)[names(v)=="ID"] <- "gridID"
  names(v)[names(v)=="area_m2"] <- "p_area_m2"

  #compute areas of each polygon and add it to v. also add unique IDs
  area <- st_area(v)
  v <- add_column(v, ID = 1:NROW(v), v_area_m2 = area, .before = 1)

  # compute weight
  weight <- v$v_area_m2/v$p_area_m2

  v <- add_column(v, weights = weight)
  #return
  return(v)
}



## somethings wrong here -> length weights are weird (max weight is 0.08). inspect. warnings?
compute_river_weights <- function(river, grid, type = "length", weight = NULL) {
  #get elements of rivers intersecting polygons
  riverIntsc <- st_contains(grid,river, sparse=FALSE)

  if(type == "length") {
    # river segment length. unit in km
    lengths <- st_length(river) %>%
      as.numeric()
    weight <- apply(riverIntsc,1, compute_weights, lengths)
    weight <- apply(weight,1, FUN=sum)
    weight <- unlist(weight)
    if (any(names(river) == "weights")) {
      message("Replacing existing 'weights' column")
      river <- select(river, -weights)
    }
    river  <- add_column(river, weights = weight)
    return(river)

  } else if(type == "equal") {
    #equal weights
    equal <- seq(1,NROW(river))
    weight <- apply(riverIntsc,1, compute_weights, equal)
    weight <- apply(weight,1, FUN=sum)
    weight <- unlist(weight)
    if (any(names(river) == "weights")) {
      message("Replacing existing 'weights' column")
      river <- select(river, -weights)
    }
    river  <- add_column(river, weights = weight)
    return(river)

  } else if(type == "strahler") {
    # to be implemented
  } else if(type == "HSupcells") {
    # to be implemented

  } else if(type == "upsegments") {
    # to be implemented

  } else if(type == "custom") {
    # to be implemented (user provides weight vector)
  } else {
    stop("Accepted values for weights are either 'length', 'equal', 'strahler', 'watershed', or 'custom'. Please check the input.")
  }
}


#helper function to get the weights
compute_weights <- function(segments, variable) {
  weights <- rep(0, length(segments))
  n <- sum(variable[segments])
  weights[segments] <- variable[segments]/n
  return(weights)

}
