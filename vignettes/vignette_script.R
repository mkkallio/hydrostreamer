## Script using SF-functions

library(raster)
library(tidyverse)
#library(RQGIS)
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
line <- compute_weights(river, grid, "length", aoi=basin, riverID="ARCID")

river <- compute_segment_runoff()
river

river <- flow_network(river, ID="ARCID")

# # year and month of timesteps (ISIMIP)
# start <- 1861
# year <- vector()
# for (i in 1:144) {
#   y <- start+i
#   current <- rep(y, 12)
#   year <- c(year, current)
# }
# month <- rep(1:12, 144)
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
names(obs)[5] <- "s451301"

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
                                              s451301 = mean(s451301, na.rm=TRUE),
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





library(hydroGOF)
library(ggplot2)
library(reshape2)
###
writepath <- "../Sub-pixel hydrology/results/"
model <- "VISIT"
resolution <- "30min"
period <- "1861-2005"
v <- st_read(paste0(writepath, "v.", resolution, ".", model,".", period, ".gpkg"))
b <- st_read(paste0(writepath, "b.", resolution, ".", model,".", period, ".gpkg"))
l <- st_read(paste0(writepath, "l.", resolution, ".", model,".", period, ".gpkg"))
e <- st_read(paste0(writepath, "e.", resolution, ".", model,".", period, ".gpkg"))
h <- st_read(paste0(writepath, "h.", resolution, ".", model,".", period, ".gpkg"))


stations <- dplyr::select(HYMOS, Code) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
gof_all <- list()
plots <- list()
for (i in 1:length(stations)) {
    station <- as.character(stations[i])
    aid <- HYMOS %>% filter(Code == station)
    id <- aid$rID_30min
    aid <- aid$ARCID
    
    station <- paste0("s",station)
    obs.ts <- dplyr::select_(obs_mon, station) %>% unlist() %>% unname()
    ts1 <- filter(e, ID == id) %>% dplyr::select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
    ts2 <- filter(h, ID == id) %>% dplyr::select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
    ts3 <- filter(l, ID == id) %>% dplyr::select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
    ts4 <- filter(v, ID == aid) %>% dplyr::select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
    ts5 <- filter(b, ID == aid) %>% dplyr::select(-ID) %>% st_set_geometry(NULL) %>% unlist() %>% unname()
    
    mat <- cbind(obs.ts,ts1,ts2,ts3,ts4,ts5)
    gof <- data.frame(equal = gof(mat[,1], mat[,2]),
                      strahler = gof(mat[,1], mat[,3]),
                      length = gof(mat[,1], mat[,4]),   
                      voronoi = gof(mat[,1], mat[,5]),
                      basin = gof(mat[,1], mat[,6]))
    
    gof_all[[i]] <- gof
    
    max <- max(mat, na.rm=TRUE)
    plot(ts1,type='l', ylim=c(0,max))
    lines(ts2, col='blue')
    lines(ts3, col='red')
    lines(ts4, col='darkgreen')
    lines(ts5, col='purple')
    points(obs.ts)
    lines(obs.ts, col='black')
    
    mat <- data.frame(Month = 1:12, mat[,1:6])
    names(mat)[2:7] <- c("Observed", "Equal", "Strahler", "Length", "Voronoi", "Drain.dir")
    ggmat <- melt(mat, id.vars="Month")

    ggobs <- data.frame(mat[,1:2])
    ggobs <- melt(ggobs, id.vars="Month")

    plots[[i]] <- ggplot(ggmat) +
            geom_line(aes(x=Month, y=value, colour=variable)) +
            geom_point(data=ggobs, aes(x=Month, y=value, fill='black')) +
            theme_minimal() +
            xlab('Month') + ylab('Q [m3/s]')
}
#gplots[[3]]
# ggsave("../Sub-pixel hydrology/poster/stat9.eps", plot = plots[[9]], device="eps")

for (i in 1:length(gof_all)) {
    if(i == 1) {
        out_table <- gof_all[[1]]
    } else {
        out_table <- data.frame(out_table, gof_all[[i]])
    }
}

write_csv(out_table, paste0(writepath, model, ".csv"))


#############
#############
# which model is best?
writepath <- "../Sub-pixel hydrology/results/"
csvlist <- list.files(writepath)
csvlist <- csvlist[grepl(".csv", csvlist) ]

for (i in 1:length(csvlist)) {
    table <- read_csv(paste0(writepath,csvlist[i], " "))
    if (i == 1) {
        stat1 <- table[,1:5]
        stat2 <- table[,6:10]
        stat3 <- table[,11:15]
        stat4 <- table[,16:20]
        stat5 <- table[,21:25]
        stat6 <- table[,26:30]
        stat7 <- table[,31:35]
        stat8 <- table[,36:40]
        stat9 <- table[,41:45]
        stat10 <- table[,46:50]
        stat11 <- table[,51:55]
    } else {
        stat1 <- cbind(stat1, table[,1:5])
        stat2 <- cbind(stat2, table[,6:10])
        stat3 <- cbind(stat3, table[,11:15])
        stat4 <- cbind(stat4, table[,16:20])
        stat5 <- cbind(stat5, table[,21:25])
        stat6 <- cbind(stat6, table[,26:30])
        stat7 <- cbind(stat7, table[,31:35])
        stat8 <- cbind(stat8, table[,36:40])
        stat9 <- cbind(stat9, table[,41:45])
        stat10 <- cbind(stat10, table[,46:50])
        stat11 <- cbind(stat11, table[,51:55])
    }
}

names <- (c(rep("CARAIB", 5), 
          rep("H08", 5),
          rep("LPJ_GUESS", 5),
          rep("LPJML", 5),
          rep("PCRGLOBWB", 5),
          rep("VISIT", 5),
          rep("WATERGAP2", 5)))
newnames <- paste0(names, names(stat1))
names(stat1) <- newnames
names(stat2) <- newnames
names(stat3) <- newnames
names(stat4) <- newnames
names(stat5) <- newnames
names(stat6) <- newnames
names(stat7) <- newnames
names(stat8) <- newnames
names(stat9) <- newnames
names(stat10) <- newnames
names(stat11) <- newnames


best <- function(stat) {
    minrmse <- min(stat[4,])
    brmse <- which(stat[4,] == minrmse)
    print(paste0("Minimum RMSE: ",minrmse," at ",names(stat)[brmse]))
    
    maxnse <- max(stat[10,])
    bnse <- which(stat[10,] == maxnse)
    print(paste0("Maximum NSE: ",maxnse," at ",names(stat)[bnse]))
    
    maxr2 <- max(stat[19,])
    br2 <- which(stat[19,] == maxr2)
    print(paste0("Maximum R2: ",maxr2," at ",names(stat)[br2]))
    
    maxcor <- max(stat[17,])
    bcor <- which(stat[17,] == maxr2)
    print(paste0("Maximum Pearson correlation: ",maxcor," at ",names(stat)[bcor]))
}






raster <- "../Sub-pixel hydrology/runoff/pcrglobwb_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
river <- "../Sub-pixel hydrology/river/Hydrosheds 3S rivers.gpkg"
river <- st_read(river)
raster <- brick(raster)

drain.dir <- "../Sub-pixel hydrology/grids/3S drdir.tif" %>%
    raster() %>% readAll()

points <- river_outlets(river,drdir)

system.time(delbas <- delineate_basin(points, drdir, riverID = "ARCID", verbose=TRUE))


system.time(delbas <- delineate_basin(points, drain.dir, riverID = "ARCID", verbose=TRUE))







library(sf)
library(raster)
library(hydrostreamer)
library(rgdal)

data(river)
data(basin)
runoff <- brick(system.file("extdata", "runoff.tif", package = "hydrostreamer"))

grid <- polygrid_timeseries(runoff, aoi=basin)
v.weights <- compute_weights(river, grid, "area", aoi=basin, riverID = "ID")
