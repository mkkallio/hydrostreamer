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



############################################
##  LOAD EVERYTHING VECTOR AND PREPROCESS ##
############################################

#polygon_file <- "grids/3S polygon intersect basin WGS84.gpkg"

river <- "devdata/HS_rivers.gpkg"
splitriver <- "devdata/HS_splitrivers.gpkg"
basin <- "../Sub-pixel hydrology/grids/HS 3S basin.gpkg"
voronoi <- "devdata/HS voronoi.gpkg"
HS_basins <- "devdata/HS_basins.gpkg"

# read and transform coordinates
river <- st_read(river)
splitriver <- st_read(splitriver)
basin <- st_read(basin)
voronoi <- st_read(voronoi)
HS_basins <- st_read(HS_basins)
HS_basins <- rename(HS_basins, ARCID = X3S_drdir)

## PROCESS ##
splitriver <- from_to_network(splitriver)
river <- from_to_network(river, ID = "ARCID")

splitriver <- river_hierarchy(splitriver)
river <- river_hierarchy(river, ID = "ARCID")

writepath <- "../Sub-pixel hydrology/results/"


#####################
## PCR-GLOBWB 30min##
#####################
model <- "PCR_GLOBWB"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff/pcrglobwb_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
raster <- crop_raster_to_watershed(raster, basin)
grid <- create_polygon_grid(raster, basin)
grid <- get_monthly_runoff(grid)



# VORONOI
voronoi <- compute_area_weights(voronoi, grid)
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, unit="mm/s")
st_write(flow.v.30min, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
# HS_BASINS
HS_basins <- compute_area_weights(HS_basins, grid)
flow.b.30min <- compute_flow_using_area(river, HS_basins, grid, unit="mm/s")
st_write(flow.b.30min, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# SEGMENT LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "length")
flow.l.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.l.30min, paste0(writepath, "l.", resolution, ".", model,".", period,".gpkg"))

# EQUAL LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "equal")
flow.e.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.e.30min, paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
splitriver <- compute_river_weights(splitriver, grid, type = "strahler")
flow.h.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.h.30min, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))



#####################
## CARAIB 30min##
#####################
model <- "CARAIB"
resolution <- "30min"
period <- "1861-2005"
raster <- "../Sub-pixel hydrology/runoff/caraib_gfdl-esm2m_historical_histsoc_co2_qtot_global_monthly_1861_2005.nc"
raster <- brick(raster)
raster <- crop_raster_to_watershed(raster, basin)
grid <- create_polygon_grid(raster, basin)
grid <- get_monthly_runoff(grid)

# VORONOI
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, unit="mm/s")
st_write(flow.v.30min, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
# HS_BASINS
flow.b.30min <- compute_flow_using_area(river, HS_basins, grid, unit="mm/s")
st_write(flow.b.30min, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# SEGMENT LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "length")
flow.l.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.l.30min, paste0(writepath, "l.", resolution, ".", model,".", period,".gpkg"))

# EQUAL LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "equal")
flow.e.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.e.30min, paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
splitriver <- compute_river_weights(splitriver, grid, type = "strahler")
flow.h.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.h.30min, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))



#####################
## H08 30min##
#####################
model <- "H08"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff/h08_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
raster <- crop_raster_to_watershed(raster, basin)
grid <- create_polygon_grid(raster, basin)
grid <- get_monthly_runoff(grid)

# VORONOI
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, unit="mm/s")
st_write(flow.v.30min, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
# HS_BASINS
flow.b.30min <- compute_flow_using_area(river, HS_basins, grid, unit="mm/s")
st_write(flow.b.30min, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# SEGMENT LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "length")
flow.l.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.l.30min, paste0(writepath, "l.", resolution, ".", model,".", period,".gpkg"))

# EQUAL LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "equal")
flow.e.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.e.30min, paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
splitriver <- compute_river_weights(splitriver, grid, type = "strahler")
flow.h.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.h.30min, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))




#####################
## LPJ GUESS 30min##
#####################
model <- "LPJ_GUESS"
resolution <- "30min"
period <- "1961-2005"
raster <- "../Sub-pixel hydrology/runoff/lpj-guess_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
raster <- crop_raster_to_watershed(raster, basin)
grid <- create_polygon_grid(raster, basin)
grid <- get_monthly_runoff(grid)

# VORONOI
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, unit="mm/s")
st_write(flow.v.30min, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
# HS_BASINS
flow.b.30min <- compute_flow_using_area(river, HS_basins, grid, unit="mm/s")
st_write(flow.b.30min, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# SEGMENT LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "length")
flow.l.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.l.30min, paste0(writepath, "l.", resolution, ".", model,".", period,".gpkg"))

# EQUAL LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "equal")
flow.e.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.e.30min, paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
splitriver <- compute_river_weights(splitriver, grid, type = "strahler")
flow.h.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.h.30min, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))





#####################
## LPJML 30min##
#####################
model <- "LPJML"
resolution <- "30min"
period <- "1861-2005"
raster <- "../Sub-pixel hydrology/runoff/lpjml_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
raster <- crop_raster_to_watershed(raster, basin)
grid <- create_polygon_grid(raster, basin)
grid <- get_monthly_runoff(grid)

# VORONOI
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, unit="mm/s")
st_write(flow.v.30min, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
# HS_BASINS
flow.b.30min <- compute_flow_using_area(river, HS_basins, grid, unit="mm/s")
st_write(flow.b.30min, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# SEGMENT LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "length")
flow.l.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.l.30min, paste0(writepath, "l.", resolution, ".", model,".", period,".gpkg"))

# EQUAL LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "equal")
flow.e.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.e.30min, paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
splitriver <- compute_river_weights(splitriver, grid, type = "strahler")
flow.h.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.h.30min, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))





#####################
## VISIT 30min##
#####################
model <- "VISIT"
resolution <- "30min"
period <- "1861-2005"
raster <- "../Sub-pixel hydrology/runoff/visit_miroc5_historical_histsoc_co2_qtot_global_monthly_1861_2005.nc"
raster <- brick(raster)
raster <- crop_raster_to_watershed(raster, basin)
grid <- create_polygon_grid(raster, basin)
grid <- get_monthly_runoff(grid)

# VORONOI
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, unit="mm/s")
st_write(flow.v.30min, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
# HS_BASINS
flow.b.30min <- compute_flow_using_area(river, HS_basins, grid, unit="mm/s")
st_write(flow.b.30min, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# SEGMENT LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "length")
flow.l.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.l.30min, paste0(writepath, "l.", resolution, ".", model,".", period,".gpkg"))

# EQUAL LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "equal")
flow.e.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.e.30min, paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
splitriver <- compute_river_weights(splitriver, grid, type = "strahler")
flow.h.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.h.30min, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))





#####################
## WATERGAP2 30min##
#####################
model <- "WATERGAP2"
resolution <- "30min"
period <- "1861-2005"
raster <- "../Sub-pixel hydrology/runoff/watergap2_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
raster <- crop_raster_to_watershed(raster, basin)
grid <- create_polygon_grid(raster, basin)
grid <- get_monthly_runoff(grid)

# VORONOI
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, unit="mm/s")
st_write(flow.v.30min, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
# HS_BASINS
flow.b.30min <- compute_flow_using_area(river, HS_basins, grid, unit="mm/s")
st_write(flow.b.30min, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# SEGMENT LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "length")
flow.l.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.l.30min, paste0(writepath, "l.", resolution, ".", model,".", period,".gpkg"))

# EQUAL LENGTH
splitriver <- compute_river_weights(splitriver, grid, type = "equal")
flow.e.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.e.30min, paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
splitriver <- compute_river_weights(splitriver, grid, type = "strahler")
flow.h.30min <- compute_flow_using_line(splitriver, grid, unit="mm/s")
st_write(flow.h.30min, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
