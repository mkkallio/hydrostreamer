## Raster to polygon grid


polygrid_timeseries <- function(brick, aoi=NULL, timesteps = NULL) {
    # see if basin exists, whether it is an 'sf' or 'sp', and crop
    if (!is.null(aoi)) {

        # if aoi is an 'sf' object, else if its an 'sp' object, else stop
        accepted <- c("sf", "sfc", "sfg")
        if( any(class(aoi) %in% accepted) ) {
            aoi <- as(aoi, "Spatial")
            brick <- raster::crop(brick, aoi, snap="out")
            aoi <- st_as_sf(aoi)
        } else if ( grepl("Spatial", class(aoi), fixed=TRUE) ) {
            brick <- raster::crop(brick, aoi)
            aoi <- st_as_sf(aoi)
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

    # change class to 'sf', intersect grid to aoi
    grid <- as(grid, "sf") %>%
        st_intersection(st_union(aoi))

    # add ID and area columns and move them in the beginning of data frame
    grid$ID <- 1:NROW(grid)
    grid$area_m2 <- st_area(grid)
    i <- grep("ID", names(grid))
    i2 <- grep("area_m2", names(grid))
    grid <- grid[, c(i, i2, (1:ncol(grid))[-c(i, i2)])]

    # assign class
    class(grid) <- append(class(grid),"HSgrid")

    #return
    return(grid)
}



# MONTHLY AVERAGE

average_monthly_runoff <- function(x, timesteps=NULL) {
  UseMethod("average_monthly_runoff", x)
}

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
      monthly_runoff <- addLayer(monthly_runoff, mrro)
    }
  }
  raster <- brick(monthly_runoff)
  return(raster)
}


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
      monthly_runoff <- addLayer(monthly_runoff, mrro)
    }
  }
  raster <- brick(monthly_runoff)
  return(raster)
}
