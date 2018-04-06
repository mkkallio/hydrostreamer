#' Polygonizes a raster and adds cell values as attribute columns to an 'sf' polygon object.
#'
#' @param brick A Raster* object.
#' @param aoi Area of interest as an 'sf' polygon object, 'SpatialPolygons', or 'SpatialPolygonsDataFrame'.
#'   Optional, but recommended for all applications.
#' @param timesteps A logical vector with same length as there are layers in brick, or a numeric vector
#'   specifying the layers to process. Optional. If not given, will process all raster layers.
#'
#' @return Returns an 'sf' polygon object, with each polygon representing a raster cell, and which has been
#'   cropped to the area of interest. Columns are ID, area of the polygon in m^2, and each timestep as separate
#'   column. Output has class 'HSgrid'.
#' @export
#'
#'
polygrid_timeseries <- function(brick, aoi=NULL, timesteps = NULL) {
    # see if aoi exists, whether it is an 'sf' or 'sp', and crop
    if (!is.null(aoi)) {

        # if aoi is an 'sf' object, else if its an 'sp' object, else stop
        accepted <- c("sf", "sfc", "sfg")
        if( any(class(aoi) %in% accepted) ) {
            aoi <- as(aoi, "Spatial")
            brick <- raster::crop(brick, aoi, snap="out")
            aoi <- sf::st_as_sf(aoi)
        } else if ( grepl("Spatial", class(aoi), fixed=TRUE) ) {
            brick <- raster::crop(brick, aoi)
            aoi <- sf::st_as_sf(aoi)
        } else {
            stop("Input area of interest should be an object of class from either 'sf' or 'sp' packages")
        }
    }

    # are timesteps given? polygonize
    if (is.null(timesteps)) {
      grid <- raster::rasterToPolygons(brick)
    } else {
      grid <- raster::rasterToPolygons(brick[[timesteps]])
    }

    # change class to 'sf', intersect grid to aoi, if exists
    if(!is.null(aoi)) {
        grid <- as(grid, "sf") %>%
            sf::st_intersection(sf::st_union(aoi))
    }


    # add ID and area columns and move them in the beginning of data frame
    grid$ID <- 1:NROW(grid)
    grid$area_m2 <- sf::st_area(grid)
    i <- which(names(grid) == "ID")
    i2 <- which(names(grid) == "area_m2")
    grid <- grid[, c(i, i2, (1:ncol(grid))[-c(i, i2)])]

    # assign class
    class(grid) <- append(class(grid),"HSgrid")

    #return
    return(grid)
}



# MONTHLY AVERAGE

#' Monthly average of a timeseries. Expects the timeseries to start from Januart and end in December.
#'
#' @param x A 'HSgrid' object obtained with polygrid_timeseries(), or a 'RasterStack', or a 'RasterBrick'.
#' @inheritParams polygrid_timeseries
#'
#' @return returns either a 'HSgrid' object with monthly summary of the timeseries, or a 'RasterBrick',
#'   depending on the class of x.
#' @export
#'
#'
average_monthly_runoff <- function(x, timesteps=NULL) {
  UseMethod("average_monthly_runoff", x)
}

#' @export
average_monthly_runoff.HSgrid <- function(grid) {
  message("Assuming first timestep to be January, and finishing in December.")
  ts <- dplyr::select(grid, -ID, -area_m2) %>% sf::st_set_geometry(NULL)
  n_ts <- NCOL(ts)
  n_year <- NCOL(ts)/12

  for (month in 1:12) {
    mts <- seq(month, (n_ts-12+month), by=12)
    mts <- ts[,mts]

    if (month == 1) {
      monthly_runoff <- apply(mts, 1, mean)
    } else {
      mrro <- apply(mts, 1, mean)
      monthly_runoff <- cbind(monthly_runoff, mrro)
    }
  }
  colnames(monthly_runoff) <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

  grid <- dplyr::select(grid, ID, area_m2)
  grid <- cbind(grid, monthly_runoff)
  return(grid)
}

#' @export
average_monthly_runoff.RasterBrick <- function(raster) {
  message("Assuming first timestep to be January, and finishing in December.")
  n_ts <- raster::nlayers(raster)
  n_year <- n_ts/12

  for (month in 1:12) {
    mts <- seq(month, (n_ts-12+month), by=12)

    if (month == 1) {
      monthly_runoff <- raster::calc(raster[[mts]], mean)
    } else {
      mrro <- raster::calc(raster[[mts]], mean)
      monthly_runoff <- raster::addLayer(monthly_runoff, mrro)
    }
  }
  raster <- raster::brick(monthly_runoff)
  return(raster)
}


#' @export
average_monthly_runoff.RasterStack <- function(raster) {
  message("Assuming first timestep to be January, and finishing in December.")
  n_ts <- raster::nlayers(raster)
  n_year <- n_ts/12

  for (month in 1:12) {
    mts <- seq(month, (n_ts-12+month), by=12)

    if (month == 1) {
      monthly_runoff <- raster::calc(raster[[mts]], mean)
    } else {
      mrro <- raster::calc(raster[[mts]], mean)
      monthly_runoff <- raster::addLayer(monthly_runoff, mrro)
    }
  }
  raster <- raster::brick(monthly_runoff)
  return(raster)
}
