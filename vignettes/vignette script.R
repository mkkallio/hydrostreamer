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
basin <- "../Sub-pixel hydrology/grids/HS 3S basin.gpkg"

# read and transform coordinates
river <- st_read(river)
basin <- st_read(basin)
raster <- brick(raster)

# # year and month of timesteps (ISIMIP)
# start <- 1860
# year <- vector()
# for (i in 1:145) {
#   y <- start+i
#   current <- rep(y, 12)
#   year <- c(year, current)
# }
# month <- rep(1:12, 145)
# timesteps <- tibble(year = year, month = month)

#crop raster to basin. May take a while if the raster is large.
raster <- crop_raster_to_watershed(raster, basin)

# polygonize raster
grid <- create_polygon_grid(raster, basin)

# transform
#river <- st_transform(river, 32647)
#grid <- st_transform(grid, 32647)
#basin <- st_transform(basin, 32647)

# compute hydrosheds watersheds ## SLOW ##
# drdir <- "../Sub-pixel hydrology/grids/3S drdir.tif" %>%
#     raster() %>%
#     mask(as(basin, "Spatial"))
#
# # points from which basins are delineated
# basin_points <- process_junctions(drdir, river)
# # delineate
# HS_basins <- delineate_basin(drdir, basin_points, ID = "ARCID")
# HS_basins_p <- rasterToPolygons(HS_basins, dissolve=TRUE)


# If wanted/needed, densify geometries BEFORE creating voronoi. function uses QGIS algorithm, which means writing file to disk,
# which means that list columns do not carry over because they are not supported.
# densification may be needed if there are long segments of rivers without nodes -> voronoi will not be adequate in such case
#river <- densify_geometry(river, 0.005)
river <- st_segmentize(river, 250)

# Create voronoi network
river <- st_transform(river, 32648)
grid <- st_transform(grid, 32648)
basin <- st_transform(basin, 32648)
voronoi <- river_voronoi(dense_river, grid, basin, ID="ARCID")
river <- st_transform(river, 4326)
grid <- st_transform(grid, 4326)
basin <- st_transform(basin, 4326)


# Split river segments at polygon borders. This is needed for weights which are based on line segments,
# but unnecessary for voronoi/watershed based weights
splitriver <- split_river_with_grid(river,grid)



# create from-to network data
splitriver <- from_to_network(splitriver)
river <- from_to_network(river, ID = "ARCID")

## 1860 ->> 1489:1740
## 1960 --> 289:540
## 1971 --> 157:420
# compute weights and accumulate voronoi flow
voronoi <- compute_area_weights(voronoi, grid)
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, timesteps=157:200)

# compute weights and accumulate length flow
splitriver <- compute_river_weights(splitriver, grid, type = "length")

no_cores <- detectCores()-1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl, cores=3)
start <- Sys.time()
flow.len.30min <- par_compute_flow_using_line(splitriver, grid, timesteps=157:420, unit="mm/s")
end <- Sys.time()
(end-start)
stopCluster(cl)

start <- Sys.time()
flow.len.30min.2 <- compute_flow_using_line(splitriver, grid, timesteps=157:420, unit="mm/s")
end <- Sys.time()
(end-start)


# compute weights and accumulate equal flow
splitriver <- river_weights(splitriver, grid, type = "equal")
flow.eq.30min <- compute_flow_using_line(splitriver, grid, timesteps=157:420, type="equal")




#saveRDS(flow.eq.30min, "flow.eq.30min.PCRGLOBWB.mrro")
#saveRDS(flow.len.30min, "flow.len.30min.PCRGLOBWB.mrro")
#saveRDS(flow.v.30min, "flow.v.30min.PCRGLOBWB.mrro")






###############################################
############# 6 MIN PCR-GLOBWB ################
###############################################


# 6min runoff
raster <- "runoff/3S 6min runoff.tif"
raster <- brick(raster)
crs(raster) <- as.character(st_crs(river))[2]
river <- "river/Hydrosheds 3S rivers.gpkg"
river <- st_read(river)


# year and month of timesteps (6min)
start <- 1971
year <- vector()
for (i in 1:50) {
  y <- start+i
  current <- rep(y, 12)
  year <- c(year, current)
}
month <- rep(1:12, 50)
timesteps <- tibble(year = year, month = month)



# polygonize raster ---- 30sec (1681 cells, 615 timesteps)
grid <- create_polygon_grid(raster)



# If wanted/needed, densify geometries BEFORE creating the from-to lists. function uses QGIS algorithm, which means writing file to disk,
# which means that list columns do not carry over because they are not supported.
# densification may be needed if there are long segments of rivers without nodes -> voronoi will not be adequate in such case
river <- densify_geometry(river, 0.005)


# Create voronoi network ---- 1.42min (2115 river segments)
voronoi <- river_voronoi(river, grid, basin, ID="ARCID")


# Split river segments at polygon borders. This is needed for weights which are based on line segments,
# but unnecessary for voronoi/watershed based weights
splitriver <- split_river_with_grid(river,grid)


# create from-to network data
splitriver <- from_to_network(splitriver)
river <- from_to_network(river, ID = "ARCID")



# compute weights and accumulate voronoi flow
voronoi <- area_weights(voronoi, grid)
flow.v.6min.PCRGLOBWB <- compute_flow_using_area(river, voronoi, grid, timesteps=289:540)

# compute weights and accumulate length flow
splitriver <- river_weights(splitriver, grid, type = "length")
flow.len.6min.PCRGLOBWB <- compute_flow_using_line(splitriver, grid, timesteps=289:540)

# compute weights and accumulate equal flow
splitriver <- river_weights(splitriver, grid, type = "equal")
flow.eq.6min.PCRGLOBWB <- compute_flow_using_line(splitriver, grid, timesteps=289:540)








##################
###### VMOD ######
##################




# # read all .asc files (VMOD output) and combine, write down
# raster <- list.files(path="runoff/3S model/subsurface", full.names = TRUE)
# raster <- stack(raster)
# crs(raster) <- CRS("+init=EPSG:32648")
# raster <- projectRaster(raster, crs=CRS('+init=EPSG:4326'))
# writeRaster(raster, "vmod_3km_qss.tif")
# qs <- "vmod_3km_qs.tif"
# qss <- "vmod_3km_qss.tif"
# qs <- brick(qs)
# qss <- brick(qss)
# runoff <- qs+qss
# writeRaster(runoff, "runoff/vmod_3km_runoff.tif")

raster <- "runoff/vmod_3km_runoff.tif"
raster <- brick(raster)



# polygonize raster
grid <- create_polygon_grid(raster, basin)

# transform
#river <- st_transform(river, 32647)
#grid <- st_transform(grid, 32647)
#basin <- st_transform(basin, 32647)


# If wanted/needed, densify geometries BEFORE creating voronoi. function uses QGIS algorithm, which means writing file to disk,
# which means that list columns do not carry over because they are not supported.
# densification may be needed if there are long segments of rivers without nodes -> voronoi will not be adequate in such case
river <- densify_geometry(river, 0.005)

# Create voronoi network
voronoi <- river_voronoi(river, grid, basin, ID="ARCID")

# Split river segments at polygon borders. This is needed for weights which are based on line segments,
# but unnecessary for voronoi/watershed based weights
splitriver <- split_river_with_grid(river,grid)



# create from-to network data
splitriver <- from_to_network(splitriver)
river <- from_to_network(river, ID = "ARCID")

## 1860 ->> 1489:1740
## 1960 --> 289:540
## 1971 --> 157:420
## vmod --> 1:252 (NULL)
# compute weights and accumulate voronoi flow
start <- Sys.time()
voronoi <- area_weights(voronoi, grid)
end <- Sys.time()
(time <- end-start)
start <- Sys.time()
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, timesteps=NULL, unit="m3/s")
end <- Sys.time()
(time <- end-start)

# compute weights and accumulate length flow
splitriver <- compute_river_weights(splitriver, grid, type = "length")

start <- Sys.time()
flow.len.30min <- compute_flow_using_line(splitriver, grid, timesteps=1:10, unit="m3/s")
end <- Sys.time()
(time1 <- end-start)

no_cores <- detectCores()-1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
start <- Sys.time()
flow.len.30min <- par_compute_flow_using_line(splitriver, grid, timesteps=1:10, unit="m3/s")
end <- Sys.time()
(time2 <- end-start)
stopCluster(cl)

# compute weights and accumulate equal flow
splitriver <- river_weights(splitriver, grid, type = "equal")
flow.eq.30min <- compute_flow_using_line(splitriver, grid, timesteps=NULL, type="equal", unit="m3/s")




#saveRDS(flow.eq.30min, "flow.eq.30min.PCRGLOBWB.mrro")
#saveRDS(flow.len.30min, "flow.len.30min.PCRGLOBWB.mrro")
#saveRDS(flow.v.30min, "flow.v.30min.PCRGLOBWB.mrro")





#################################
###### PLOT FLOWS TOGETHER ######
#################################

library(mapview)
library(readr)

### load desired flow object ...
# load

# load observations
obs <- "obs/3S_monthly_Q.csv"
obs <- read_csv(obs)

# load station data
HYMOS <- "obs/HYMOS stations.gpkg"
HYMOS <- st_read(HYMOS)

# explore hymos station locations and rivers using mapview()
#map <- mapview(river)
#(map <- mapview(HYMOS, map=map))
#map <- mapview(splitriver)
#(map <- mapview(HYMOS, map=map))







station <- "s440201"
rID <- 848656
Q <- flow.v.30min
plotFlow(station, rID, Q, obs, ARCID=TRUE, timeseries=NULL)
