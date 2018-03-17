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
voronoi <- "devdata/HS voronoi.gpkg"
HS_basins <- "devdata/HS_basins.gpkg"

# read and transform coordinates
river <- st_read(river)
basin <- st_read(basin)
raster <- brick(raster)
voronoi <- st_read(voronoi)
HS_basins <- st_read(HS_basins)
HS_basins <- rename(HS_basins, ID = X3S_drdir)


drdir <- "../Sub-pixel hydrology/grids/3S drdir.tif" %>%
     raster() %>%
     mask(as(basin, "Spatial"))

grid <- polygrid_timeseries(raster, aoi=basin)
area <- compute_weights(river, grid, "area", aoi=basin,drain.dir = drdir, riverID="ID")
line <- compute_weights(river, grid, "strahler", aoi=basin, ID="ARCID")

river <- compute_segment_runoff()
river

river <- flow_network(river, ID="ARCID")

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
# HS_basins <- st_as_sf(HS_basins_p)


# If wanted/needed, densify geometries BEFORE creating voronoi. function uses QGIS algorithm, which means writing file to disk,
# which means that list columns do not carry over because they are not supported.
# densification may be needed if there are long segments of rivers without nodes -> voronoi will not be adequate in such case
#river <- densify_geometry(river, 0.005)
river <- st_segmentize(river, 250)

# Create voronoi network
river <- st_transform(river, 32648)
grid <- st_transform(grid, 32648)
basin <- st_transform(basin, 32648)
voronoi <- river_voronoi(river, grid, basin, ID="ARCID")
river <- st_transform(river, 4326)
grid <- st_transform(grid, 4326)
basin <- st_transform(basin, 4326)
voronoi <- st_transform(voronoi, 4326)


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
flow.v.30min <- compute_flow_using_area(river, voronoi, grid, timesteps=157:420, unit="mm/s")

HS_basins <- compute_area_weights(HS_basins, grid)
flow.b.30min <- compute_flow_using_area(river, HS_basins, grid, timesteps=157:420, unit="mm/s")

# compute weights and accumulate length flow
splitriver <- compute_river_weights(splitriver, grid, type = "length")

#no_cores <- detectCores()-1
#cl <- makePSOCKcluster(no_cores)
#registerDoParallel(cl, cores=3)
start <- Sys.time()
flow.len.30min <- par_compute_flow_using_line(splitriver, grid, timesteps=157:420, unit="mm/s")
end <- Sys.time()
(end-start)
#stopCluster(cl)

start <- Sys.time()
flow.len.30min <- compute_flow_using_line(splitriver, grid, timesteps=157:420, unit="mm/s")
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
obs <- "../Sub-pixel hydrology/obs/3S_monthly_Q.csv"
obs <- read_csv(obs)

# load station data
HYMOS <- "../Sub-pixel hydrology/obs/HYMOS stations.gpkg"
HYMOS <- st_read(HYMOS)

# explore hymos station locations and rivers using mapview()
#map <- mapview(river)
#(map <- mapview(HYMOS, map=map))
#map <- mapview(splitriver)
#(map <- mapview(HYMOS, map=map))

obs_na <- obs == 0
obs_mon <- obs
obs_mon[obs_na] <- NA
obs_mon <- group_by(obs, Month) %>% summarise(s450701 = mean(s450701, na.rm=TRUE),
                                              s451305 = mean(s451305, na.rm=TRUE),
                                              s450101 = mean(s450101, na.rm=TRUE),
                                              s440601 = mean(s440601, na.rm=TRUE),
                                              s440201 = mean(s440201, na.rm=TRUE),
                                              s440103 = mean(s440103, na.rm=TRUE),
                                              s440102 = mean(s440102, na.rm=TRUE),
                                              s430106 = mean(s430106, na.rm=TRUE),
                                              s430105 = mean(s430105, na.rm=TRUE),
                                              s430103 = mean(s430103, na.rm=TRUE),
                                              s430101 = mean(s430101, na.rm=TRUE))


obs_mon <- obs_mon[c(5,4,8,1,9,7,6,2,12,11,10,3), ]




station <- "s440601"

rID <- 1157
Q <- flow.h.30min
plot_flow(station, rID, Q, obs_mon, ARCID=FALSE, timeseries=NULL)


rID <- 848656
Q <- flow.b.30min
plot_flow(station, rID, Q, obs_mon, ARCID=FALSE, timeseries=NULL)




# compare two
ts1 <- filter(flow.v, ID == 853219) %>% select(-ID) %>% t() %>% unlist()
ts2 <- filter(flow.b, ID == 853219) %>% select(-ID) %>% t() %>% unlist()
ts3 <- filter(flow.l, ID == 1224) %>% select(-ID) %>% t() %>% unlist()
ts4 <- filter(flow.e, ID == 1224) %>% select(-ID) %>% t() %>% unlist()
ts5 <- filter(flow.h, ID == 1224) %>% select(-ID) %>% t() %>% unlist()

plot(ts1, type='l')
lines(ts2, col='blue', lty=2)
lines(ts3, col='red')
lines(ts4, col='pink')
lines(ts5, col='orange')

cor(cbind(ts1,ts2,ts3,ts4,ts5))



###
writepath <- "../Sub-pixel hydrology/results/"
model <- "CARAIB"
resolution <- "30min"
period <- "1861-2005"
v <- st_read(paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
b <- st_read(paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
l <- st_read(paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
e <- st_read(paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))
h <- st_read(paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))


station <- "440201"
aid <- HYMOS %>% filter(Code == station)
id <- aid$rID_30min
aid <- aid$ARCID

station <- paste0("s",station)
obs.ts <- select_(obs_mon, station) %>% unlist() %>% unname()
ts1 <- filter(e, ID == id) %>% select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
ts2 <- filter(h, ID == id) %>% select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
ts3 <- filter(l, ID == id) %>% select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
ts4 <- filter(v, ID == aid) %>% select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
ts5 <- filter(b, ID == aid) %>% select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()

cor(cbind(obs.ts,ts1,ts2,ts3,ts4,ts5))
max <- max(cbind(obs.ts,ts1,ts2,ts3,ts4,ts5))

plot(ts1,type='l', ylim=c(0,max))
lines(ts2, col='blue')
lines(ts3, col='red')
lines(ts4, col='darkgreen')
lines(ts5, col='purple')
points(obs.ts)
