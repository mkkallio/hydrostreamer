### testing

library(sf)
library(raster)
library(hydrostreamer)

river <- "devdata/HS_rivers.gpkg"
basin <- "devdata/testibasin.gpkg"
raster <- "../Sub-pixel hydrology/runoff/pcrglobwb_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
dem <- "devdata/3S dem clipped.tif"

# read and transform coordinates
basin <- st_read(basin)
river <- st_read(river)
runoff <- brick(raster)
dem <- raster(dem)

int <- river %>% st_intersects(st_geometry(basin), sparse=FALSE)
river <- river[int,]


grid <- polygrid_timeseries(runoff, aoi=basin) %>% average_monthly_runoff()


voronoi <- river_voronoi(river, aoi=basin, riverID="ARCID")

b.weights <- compute_weights(river, grid, "area", aoi=basin, basins = voronoi, riverID="ARCID")
names(b.weights[[2]])
l.weights <- compute_weights(river, grid, "length", aoi=basin, riverID="ARCID")
names(l.weights[[2]])

flow.b <- compute_segment_runoff(b.weights)
flow.l <- compute_segment_runoff(l.weights)

l.weights
l.weights[[1]]
summary(flow.l[,10])
names(flow.l)
plot(flow.b[,10:13])
summary(flow.b[10:13])
plot(flow.b)
plot(voronoi)



splitriver <- split_river_with_grid(river, grid)
splitriver2 <- split_river_with_grid(river, grid, ID="ARCID")

river <- flow_network(river, riverID = "ARCID", verbose = FALSE)

drain.dir <- raster("devdata/testidrdir.tif")
drain.dir <- readAll(drain.dir)
points <- river_outlets(river, drain.dir)
ts <- Sys.time()
testi <- delineate_basin_up(drain.dir, points, riverID="ARCID", verbose=T)
te <- Sys.time()

print(te-ts)



system.time(testi <- delineate_basin(points, drain.dir, riverID="ARCID", verbose=T))
system.time(testi <- delineate_basin3(points, drain.dir, riverID="ARCID", verbose=T))
##################
##################
##################
library(sf)
library(raster)
library(hydrostreamer)
library(dplyr)
library(inline)

river <- "devdata/HS_rivers.gpkg"
basin <- "devdata/testibasin.gpkg"
raster <- "../Sub-pixel hydrology/runoff/pcrglobwb_gfdl-esm2m_hist_nosoc_mrro_monthly_1971_2005.nc"
dem <- "devdata/3S dem clipped.tif"

# read and transform coordinates
basin <- st_read(basin)
river <- st_read(river)
int <- river %>% st_intersects(st_geometry(basin), sparse=FALSE)
river <- river[int,]
dem <- raster(dem)
drain.dir <- raster("devdata/testidrdir.tif")
drain.dir <- readAll(drain.dir)
points <- river_outlets(river, drain.dir)

nx <- nrow(drain.dir)
ny <- ncol(drain.dir)
nseeds <- nrow(points)
seeds <- raster::cellFromXY(drain.dir, sf::st_coordinates(points))
basID <- dplyr::select(points, ARCID) %>% st_set_geometry(NULL) %>% unlist
drdir <- values(drain.dir)
delbas <- vector("numeric", ncell(drain.dir))

dyn.load("src/delin.dll")

.Fortran("delineate", nx, ny, nseeds, seeds, 
         basID, drdir, drdir)

out <- .Fortran("delineate", as.integer(nx), as.integer(ny), as.integer(nseeds), as.integer(seeds), 
         as.integer(basID), as.integer(drdir), as.integer(delbas))


out <- delineate(nx, ny, nseeds, seeds, basID, drdir, drdir)
out
