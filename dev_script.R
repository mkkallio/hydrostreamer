# Hydrostreamer application in Muriae basin, Brazil
system.time({
library(hydrostreamer)
library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(stringr)

#################
# Load data
    system.time({
# river
#river <- st_read("../../GLOBAL RIVER CLASSIFICATION/GloRic_v10_shapefile/GloRiC_v10.shp")
#aoi <- st_read("data/muriae_basin.shp")
#river <- st_intersection(river, aoi %>% st_transform(4326))
#st_write(river, "data/muriae_rivers.gpkg")
river <- read_sf("../hydrostreamer pojects/Muriae basin/Data/muriae_rivers.gpkg")
aoi <- read_sf("../hydrostreamer pojects/Muriae basin/Data/muriae_basin.shp") %>% 
    st_transform(4326)


# runoff rasters and naming
rasters <- list.files("../RUNOFF DATA/ISIMIP 2a varsoc/", full.names = TRUE)
remove <- grepl(".aux.xml", rasters)
rasters <- rasters[!remove]
remove <- grepl("watch", rasters)
rasters <- rasters[!remove]
fcast_model <- vector("character", length(rasters))
fcast_reanalysis <- vector("character", length(rasters))
fcast_names <- vector("character", length(rasters))
fcast_startdate <- vector("character", length(rasters))
for (i in seq_along(rasters)) {
    temp <- word(rasters[[i]], 4, sep="/")
    fcast_model[[i]] <- word(temp,1,sep = "_")
    fcast_reanalysis[[i]] <- word(temp,2,sep = "_")
    fcast_names[[i]] <- paste(fcast_model[[i]], fcast_reanalysis[[i]], sep="_")
    fcast_startdate[[i]] <- paste0(word(temp,9,sep = "_"),"-01-01" )
}
fcast_startdate <- lubridate::date(fcast_startdate)
}) #### 0.06s
    
    
###############
# Read observations
obs <- readr::read_delim("../hydrostreamer pojects/Muriae basin/Data/muriae_discharge_data.txt", delim=" ")
colnames(obs)[1] <- "Date"

monthly_obs <- obs %>% 
    mutate(year = year(Date),
           month = month(Date)) %>%
    group_by(year,month) %>%
    summarise_all(mean, na.rm=TRUE) %>%
    mutate(Date = ymd(paste(year,month,"01", sep="-"))) %>%
    ungroup %>%
    select(Date, everything(),-year, -month)
    




###############
# hydrostreamer
system.time({ 
    HS <- raster_to_HSgrid(rasters, 
                           fcast_startdate,
                           "month",
                           aoi,
                           fcast_names) %>%
        compute_HSweights(river, 
                           weights="length", 
                           aoi=aoi, 
                           riverID = "Reach_ID") %>%
        downscale_runoff() %>%
        add_observations(monthly_obs,
                         c(61311019,61315260,
                           61307950,61311778,
                           61316367,61320749)) %>%
        accumulate_runoff(method = "simple", 
                          velocity = 1) 
})



###############
# OR STEP-BY-STEP

###############
# Create HSgrid
system.time({
HSgrid <- raster_to_HSgrid(brick(rasters[1]), 
                           lubridate::ymd(fcast_startdate[[1]]), 
                           timestep="month", 
                           aoi=aoi, 
                           name=fcast_names[1])

###############
# Add additional layers to HSgrid
for(i in 2:length(rasters)) {
    HSgrid <- add_HSgrid(HSgrid,
                         raster = brick(rasters[i]),
                         date = lubridate::ymd(fcast_startdate[[i]]),
                         timestep="month",
                         aoi=aoi,
                         name=fcast_names[i])
}

HSgrid <- add_HSgrid(HSgrid, 
                     raster=brick("../RUNOFF DATA/LORA 1.0/lora combined.tif"),
                     date = lubridate::ymd("1980-01-01"), 
                     timestep="month", 
                     aoi = aoi,
                     name = "LORA")

}) ### 66 sec
#################
# Create HSweights object using river segment length weighting
system.time({
HSweights.monthly <- compute_HSweights(river, 
                                      HSgrid, 
                                      weights="length", 
                                      aoi=aoi, 
                                      riverID = "Reach_ID")
}) ### 27 sec
##################
# Downscale
system.time({
HSrunoff.monthly <- downscale_runoff(HSweights.monthly)
}) ### 6.22 sec

###############
# Apply river routing
#system.time(HSflow.inst <- accumulate_runoff(HSrunoff.monthly, 
#                                               method = "instant")) ## 3.60 sec
system.time(HSflow.simple <- accumulate_runoff(HSrunoff.monthly, 
                                                method = "simple", 
                                                velocity = 1)) ## 131 sec
# system.time(HSflow.musk <- accumulate_runoff(HSrunoff.monthly, 
#                                                 method = "simple", 
#                                                 velocity = 1,
#                                                 x = 1)) ### 157 sec



})

###############
# Read observations and create HSobs object
obs <- read_delim("data/muriae_discharge_data.txt", delim=" ")
colnames(obs)[1] <- "Date"

monthly_obs <- obs %>% 
    mutate(year = year(Date),
           month = month(Date)) %>%
    group_by(year,month) %>%
    summarise_all(mean, na.rm=TRUE) %>%
    mutate(Date = ymd(paste(year,month,"01", sep="-"))) %>%
    ungroup %>%
    dplyr::select(Date, everything(),-year, -month)

HSflow.simple <- add_observations(HSflow.simple, monthly_obs,
                                  c(61311019,61315260,61307950,61311778,61316367,61320749))
HSrunoff.monthly <- add_observations(HSrunoff.monthly, monthly_obs,
                                  c(61311019,61315260,61307950,61311778,61316367,61320749))



###############
# Optimise flow @ observation stations using Ordinary Least Squares combination
optim <- optimise_point(HSflow.simple,
                        optim_method = "CLS", 
                        combination = "mon",
                        train = 0.99)
gof <- as.data.frame(matrix(NA, ncol=6,nrow=20))
for(i in seq_along(optim)) {
    gof[,i] <- optim[[i]]$Goodness_of_fit$`Entire timeseries`
}
colnames(gof) <- HSflow.simple$observation_station[!is.na(HSflow.simple$observation_station)]
rownames(gof) <- rownames(optim[[1]]$Goodness_of_fit)
gof_ols <- gof


plot(optim$`58917000`$Optimized_ts$Date,
     optim$`58917000`$Optimized_ts$Observations, 
     type='l', col='red', ylab='Q m3/s')
lines(optim$`58917000`$Optimized_ts$Date,
      optim$`58917000`$Optimized_ts$Optimized) 
title("Obs (red) and hydrostreamer optimized (black) monthly flow. 
      Station 58917000. KGE = 0.96")


optimized_cls_ts <- optimise_region(HSrunoff.monthly, train = 0.99, verbose=TRUE)
optimized_cls_mon <- optimise_region(HSrunoff.monthly, 
                                     train = 0.99, 
                                     verbose=TRUE,
                                     combination = "mon")

optimized_ols_ts <- optimise_region(HSrunoff.monthly, 
                                     train = 0.99, 
                                     verbose=TRUE,
                                     combination = "ts",
                                     optim_method = "OLS",
                                     drop=FALSE)

plot(optimized_ols_ts$observation_ts[[412]]$Date,
     optimized_ols_ts$observation_ts[[412]]$observations, 
     type='l', col='red', ylab='Q m3/s')
lines(optimized_ols_ts$discharge_ts[[412]]$Date,
      optimized_ols_ts$discharge_ts[[412]]$Optimized) 










##### muriae swat

# read swat output table (discharge)
library(readr)
swat_output <- read_delim("C:/Users/haiwe/OneDrive - Aalto-yliopisto/Aalto/PhD/Data and scripts/hydrostreamer pojects/Muriae basin/swat_data/output_rch.txt",
                         delim = " ")
names(swat_output)[1] <- "Date"

###### 
# because the output is in discharge, we must somehow estimate runoff contrubuted
# by each subbasin separately. Here we do that by dividing discharge by the upstream
# area.

# read in the subbasins 
runoff_basin <- read_sf("C:/Users/haiwe/OneDrive - Aalto-yliopisto/Aalto/PhD/Data and scripts/hydrostreamer pojects/Muriae basin/swat_data/subbasins.shp") %>%
    st_transform(4326)
runoff_basin <- runoff_basin %>%
    dplyr::mutate(gridID = Subbasin)

# read in rivers 
swat_river <- read_sf("C:/Users/haiwe/OneDrive - Aalto-yliopisto/Aalto/PhD/Data and scripts/hydrostreamer pojects/Muriae basin/swat_data/rivers.shp")
#route
swat_river <- river_network(swat_river, riverID = "Subbasin")

basin_area <- vector("numeric", nrow(runoff_basin))
for (i in 1:nrow(runoff_basin)) {
    upstream <- upstream(swat_river, runoff_basin$Subbasin[i])
    bas <- runoff_basin %>%
        filter(Subbasin %in% upstream$riverID)
    area <- st_union(bas) %>% st_area
    
    col <- which(colnames(swat_output) == runoff_basin$Subbasin[i])
    swat_output[,col] <- unlist(swat_output[,col]) / (area / 1000)
    
    #basin_area[i] <- area 
}

summary(swat_output)


HSgrid <- create_HSgrid(runoff_basin, swat_output, name = "swat")


# read river
river <- st_read("C:/Users/haiwe/OneDrive - Aalto-yliopisto/Aalto/PhD/Data and scripts/hydrostreamer pojects/Muriae basin/Data/muriae_rivers.gpkg")
aoi <- st_read("C:/Users/haiwe/OneDrive - Aalto-yliopisto/Aalto/PhD/Data and scripts/hydrostreamer pojects/Muriae basin/data/muriae_basin.shp") %>% 
    st_transform(4326)


#################
# Create HSweights object using river segment length weighting
system.time({
    HSweights <- compute_HSweights(river, 
                                           HSgrid, 
                                           weights="length", 
                                           aoi=aoi, 
                                           riverID = "Reach_ID")
}) ### 37 sec
##################
# Downscale
system.time({
    HSrunoff <- downscale_runoff(HSweights)
}) ### 4.6 sec

### 172 sec
system.time(HSflow <- accumulate_runoff(HSrunoff, 
                                        method = "simple", 
                                        velocity = 1))

obs <- read_delim("C:/Users/haiwe/OneDrive - Aalto-yliopisto/Aalto/PhD/Data and scripts/hydrostreamer pojects/Muriae basin/Data/muriae_discharge_data.txt", delim=" ")
colnames(obs)[1] <- "Date"

HSflow <- add_observations(HSflow, obs,
                     c(61311019,61315260,61307950,61311778,61316367,61320749))


flow_gof(HSflow) %>% select(Prediction, Station, ME, `PBIAS %`, NSE, KGE, R2)
