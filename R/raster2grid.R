## Raster to polygon grid


crop_raster_to_watershed <- function(raster, area) {
  p4s <- projection(raster)
  ext <- extent(raster) %>%
    as('SpatialPolygons') %>%
    st_as_sf() %>%
    st_set_crs(p4s)
  grid <- st_make_grid(ext, n = c(raster@ncols, raster@nrows), what="polygons")
  intsc <- st_intersects(grid, area, sparse=FALSE)
  grid <- grid[intsc]
  bbox <- as.vector(st_bbox(grid))
  raster <- crop(raster, bbox[c(1,3,2,4)])
  return(raster)
}


create_polygon_grid <- function(raster, basin, timesteps = NULL) {
  if (is.null(timesteps)) {
    grid <- rasterToPolygons(raster)
  } else {
    grid <- rasterToPolygons(raster[[timesteps]])
  }

  #cells <- cellFromPolygon(raster, grid) %>% unlist()
  grid <- as(grid, "sf") #%>% st_set_crs(as.character(crs(raster)))
  intersects <- suppressWarnings(suppressMessages(st_intersects(grid, basin, sparse=FALSE)))
  grid <- st_buffer(grid, dist=0)
  int.area <- suppressWarnings(suppressMessages(st_intersection(grid, st_geometry(basin)))) %>%
      st_area() %>%
      unclass()
  areas <- vector("numeric", length(intersects))
  areas[intersects] <- int.area

  #add ID and area columns and move them in the beginning of data frame
  grid$ID <- 1:NROW(grid)
  grid$area_m2 <- areas
  i <- grep("ID", names(grid))
  i2 <- grep("area_m2", names(grid))
  grid <- grid[, c(i, i2, (1:ncol(grid))[-c(i, i2)])]

  #return
  return(grid)
}
