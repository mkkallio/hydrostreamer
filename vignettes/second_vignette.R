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

river <- "../hydroc2s/devdata/HS_rivers.gpkg"
splitriver <- "../hydroc2s/devdata/HS_splitrivers.gpkg"
basin <- "../Sub-pixel hydrology/grids/HS 3S basin.gpkg"
voronoi <- "../hydroc2s/devdata/HS voronoi.gpkg"
HS_basins <- "../hydroc2s/devdata/HS_basins.gpkg"

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
period <- "ts"
raster <- "../Sub-pixel hydrology/runoff/pcrglobwb_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff(grid)

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
period <- "ts"
raster <- "../Sub-pixel hydrology/runoff/caraib_gfdl-esm2m_historical_histsoc_co2_qtot_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff(grid)

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
period <- "ts"
raster <- "../Sub-pixel hydrology/runoff/h08_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff(grid)

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
period <- "ts"
raster <- "../Sub-pixel hydrology/runoff/lpj-guess_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff(grid)

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
period <- "ts"
raster <- "../Sub-pixel hydrology/runoff/lpjml_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff(grid)

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
period <- "ts"
raster <- "../Sub-pixel hydrology/runoff/visit_miroc5_historical_histsoc_co2_qtot_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, timesteps=1477:1728 , aoi=basin)
#grid <- average_monthly_runoff(grid)

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
period <- "ts"
raster <- "../Sub-pixel hydrology/runoff/watergap2_gfdl-esm2m_historical_histsoc_co2_qs_global_monthly_1861_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff(grid)

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
flow.v <- compute_runoff_using_area(v.weights[[1]], v.weights[[2]], v.weights[[3]], rID = "ID", bID = "rID", timesteps = NULL, unit = "m3/s")
#flow.v <- compute_segment_runoff(v.weights, unit="m3/s")
flow.v <- accumulate_flow(flow.v)
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))

# HS_BASINS
flow.b <- compute_runoff_using_area(b.weights[[1]], b.weights[[2]], b.weights[[3]], unit = "m3/s")
flow.b <- accumulate_flow(flow.b)
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))

# EQUAL WEIGHT
flow.e <- compute_runoff_using_line(e.weights[[1]], e.weights[[2]], unit="m3/s")
flow.e <- accumulate_flow(flow.e)
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))

# SEGMENT LENGTH
flow.l <- compute_runoff_using_line(l.weights[[1]], l.weights[[2]], unit="m3/s")
flow.l <- accumulate_flow(flow.l)
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))

# STREAM ORDER
flow.h <- compute_runoff_using_line(h.weights[[1]], h.weights[[2]], unit="m3/s")
flow.h <- accumulate_flow(flow.h)
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))




##############
## NEW ONES ##
writepath <- "../Sub-pixel hydrology/results2/"


#####################
## PCR-GLOBWB 30min##
#####################
model <- "ftPCRGLOBWB"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff2/pcrglobwb_gfdl-esm2m_hist_pressoc_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))



#####################
## CARAIB 30min##
#####################
model <- "ftCARAIB"
resolution <- "30min"
period <- "1971-2010"
raster <- "../Sub-pixel hydrology/runoff2/caraib_gswp3_hist_pressoc_co2_mrro_global_monthly_1971_2010.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid

# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))






#####################
## H08 30min##
#####################
model <- "ftH08"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff2/h08_gfdl-esm2m_hist_pressoc_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid

# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))


#####################
## LPJML 30min##
#####################
model <- "ftLPJML"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff2/lpjml_gfdl-esm2m_hist_pressoc_co2_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))





#####################
## DBH 30min##
#####################
model <- "ftDBH"
resolution <- "30min"
period <- "1971-2010"
raster <- "../Sub-pixel hydrology/runoff2/lpjml_gfdl-esm2m_hist_pressoc_co2_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid

# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))





#####################
## MPIHM 30min##
#####################
model <- "ftMPIHM"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff2/lpjml_gfdl-esm2m_hist_pressoc_co2_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid

# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))



#####################
## VIC 30min##
#####################
model <- "ftVIC"
resolution <- "30min"
period <- "1971-2005"
raster <- "../Sub-pixel hydrology/runoff2/lpjml_gfdl-esm2m_hist_pressoc_co2_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid

# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))




#####################
## WATERGAP 30min##
#####################
model <- "ftWATERGAP"
resolution <- "30min"
period <- "1971-2010"
raster <- "../Sub-pixel hydrology/runoff2/lpjml_gfdl-esm2m_hist_pressoc_co2_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid

# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))



#####################
## WBM 30min##
#####################
model <- "ftWBM"
resolution <- "30min"
period <- "1971-2010"
raster <- "../Sub-pixel hydrology/runoff2/lpjml_gfdl-esm2m_hist_pressoc_co2_mrro_monthly_1971_2005.nc"
raster <- brick(raster)
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff.HSgrid(grid)

v.weights[[3]] <- grid
b.weights[[3]] <- grid
e.weights[[2]] <- grid
l.weights[[2]] <- grid
h.weights[[2]] <- grid


# VORONOI
flow.v <- compute_segment_runoff(v.weights)
flow.v <- accumulate_flow(flow.v)
#st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.v, paste0(writepath, "v.", resolution, ".", model,".", "ts.gpkg"))

# HS_BASINS
flow.b <- compute_segment_runoff(b.weights)
flow.b <- accumulate_flow(flow.b)
#st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.b, paste0(writepath, "b.", resolution, ".", model,".", "ts.gpkg"))

# EQUAL WEIGHT
flow.e <- compute_segment_runoff(e.weights)
flow.e <- accumulate_flow(flow.e)
#st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", period,".gpkg"))
st_write(flow.e, paste0(writepath, "e.", resolution, ".", model,".", "ts.gpkg"))

# SEGMENT LENGTH
flow.l <- compute_segment_runoff(l.weights)
flow.l <- accumulate_flow(flow.l)
#st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.l, paste0(writepath, "l.", resolution, ".", model,".", "ts.gpkg"))

# STREAM ORDER
flow.h <- compute_segment_runoff(h.weights)
flow.h <- accumulate_flow(flow.h)
#st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))
st_write(flow.h, paste0(writepath, "h.", resolution, ".", model,".", "ts.gpkg"))









#####################
## PCR-GLOBWB 6min ##
#####################
writepath <- "../Sub-pixel hydrology/results/"
model <- "PCRGLOBWB"
resolution <- "6min"
period <- "1960-2011"
raster <- "../Sub-pixel hydrology/runoff/3S 6min runoff.tif"
raster <- brick(raster)
crs(raster) <- CRS("+init=epsg:4326")
grid <- polygrid_timeseries(raster, aoi=basin)
#grid <- average_monthly_runoff(grid)

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

