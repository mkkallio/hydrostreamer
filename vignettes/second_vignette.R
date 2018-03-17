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
#splitriver <- st_read(splitriver)
basin <- st_read(basin)
voronoi <- st_read(voronoi) %>% st_transform(4326)
voronoi <- rename(voronoi, ID = ARCID)
HS_basins <- st_read(HS_basins)
HS_basins <- rename(HS_basins, ID = X3S_drdir)

writepath <- "../Sub-pixel hydrology/results/"


#####################
## PCR-GLOBWB 30min##
#####################
model <- "PCRGLOBWB"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff/pcrglobwb_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights <- compute_weights(river, grid, "area", aoi=basin, basins = voronoi, riverID="ARCID")
b.weights <- compute_weights(river, grid, "area", aoi=basin, basins = HS_basins, riverID="ARCID")
e.weights <- compute_weights(river, grid, "equal", aoi=basin, riverID="ARCID")
l.weights <- compute_weights(river, grid, "length", aoi=basin, riverID="ARCID")
h.weights <- compute_weights(river, grid, "strahler", aoi=basin, riverID="ARCID")


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))



#####################
## CARAIB 30min##
#####################
model <- "CARAIB"
resolution <- "30min"
period <- "1861-2005"
raster <- "../Sub-pixel hydrology/runoff/caraib_gfdl-esm2m_historical_histsoc_co2_qtot_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))







#####################
## H08 30min##
#####################
model <- "H08"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff/h08_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))






#####################
## LPJ GUESS 30min##
#####################
model <- "LPJ_GUESS"
resolution <- "30min"
period <- "1961-2005"
raster <- "../Sub-pixel hydrology/runoff/lpj-guess_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))






#####################
## LPJML 30min##
#####################
model <- "LPJML"
resolution <- "30min"
period <- "1861-2005"
raster <- "../Sub-pixel hydrology/runoff/lpjml_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))




#####################
## VISIT 30min##
#####################
model <- "VISIT"
resolution <- "30min"
period <- "1861-2005"
raster <- "../Sub-pixel hydrology/runoff/visit_miroc5_historical_histsoc_co2_qtot_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))





#####################
## WATERGAP2 30min##
#####################
model <- "WATERGAP2"
resolution <- "30min"
period <- "1861-2005"
raster <- "../Sub-pixel hydrology/runoff/watergap2_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))







#####################
## PCR-GLOBWB 6min ##
#####################
model <- "PCRGLOBWB"
resolution <- "6min"
period <- "1960-2011"
raster <- "../Sub-pixel hydrology/runoff/3S 6min runoff.tif"
raster <- brick(raster)
crs(raster) <- CRS("+init=epsg:4326")
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights <- compute_weights(river, grid, "area", aoi=basin, basins = voronoi, riverID="ARCID")
b.weights <- compute_weights(river, grid, "area", aoi=basin, basins = HS_basins, riverID="ARCID")
e.weights <- compute_weights(river, grid, "equal", aoi=basin, riverID="ARCID")
l.weights <- compute_weights(river, grid, "length", aoi=basin, riverID="ARCID")
h.weights <- compute_weights(river, grid, "strahler", aoi=basin, riverID="ARCID")


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))


#####################
## VMOD 3km ##
#####################
model <- "VMOD"
resolution <- "3km"
period <- "1985-2005"
raster <- "../Sub-pixel hydrology/runoff/vmod_3km_runoff.tif"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
grid <- average_monthly_runoff(grid)

v.weights <- compute_weights(river, grid, "area", aoi=basin, basins = voronoi, riverID="ARCID")
b.weights <- compute_weights(river, grid, "area", aoi=basin, basins = HS_basins, riverID="ARCID")
e.weights <- compute_weights(river, grid, "equal", aoi=basin, riverID="ARCID")
l.weights <- compute_weights(river, grid, "length", aoi=basin, riverID="ARCID")
h.weights <- compute_weights(river, grid, "strahler", aoi=basin, riverID="ARCID")


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
