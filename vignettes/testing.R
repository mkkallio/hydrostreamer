## Script using SF-functions

library(raster)
library(tidyverse)
library(RQGIS)
library(sf)
library(geosphere)

source("R/compute_flow.R")
source("R/diagnosis.R")
source("R/create_river_voronoi.R")
source("R/compute_weights.R")
source("R/process_river.R")
source("R/raster2grid.R")
source("R/delineate_watershed.R")

#polygon_file <- "grids/3S polygon intersect basin WGS84.gpkg"
raster <- "../Sub-pixel hydrology/runoff/pcrglobwb_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
river <- "../Sub-pixel hydrology/river/Hydrosheds 3S rivers.gpkg"
#basin <- "../Sub-pixel hydrology/grids/HS 3S basin.gpkg"
basin <- "devdata/testibasin.gpkg"


# read and transform coordinates
river <- st_read(river)
basin <- st_read(basin)
raster <- brick(raster)
drdir <- "../Sub-pixel hydrology/grids/3S drdir.tif" %>%
  raster() %>%
  mask(as(basin, "Spatial"))

grid <- polygrid_timeseries(raster, aoi=basin)
area <- compute_weights(river, grid, "area", aoi=basin, riverID="ARCID")
line <- compute_weights(river, grid, runif(63), aoi=basin, riverID="ARCID")

river <- compute_segment_runoff()

vorun <- compute_segment_runoff(area)


start <- Sys.time()
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)
( time_grid <- Sys.time()-start )

start <- Sys.time()
ras <- average_monthly_runoff(raster)
grid2 <- polygrid_timeseries(ras, aoi=basin)
( time_grid2 <- Sys.time()-start )
