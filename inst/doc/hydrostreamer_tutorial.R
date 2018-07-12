## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(sf)
library(raster)
library(hydrostreamer)
library(rgdal)

data(river)
data(basin)
runoff <- brick(system.file("extdata", "runoff.tif", package = "hydrostreamer"))

plot(runoff[[1]])
plot(st_geometry(river), add=TRUE)
plot(st_geometry(basin), add=TRUE)

## ----message=FALSE, warning=FALSE----------------------------------------
grid <- polygrid_timeseries(runoff, aoi=basin)
names(grid)
plot(grid[,"area_m2"])

## ----message=FALSE, warning=FALSE----------------------------------------
voronoi <- river_voronoi(river, aoi=basin, riverID = "ID")
plot(voronoi[,"riverID"], reset=FALSE)
plot(st_geometry(river), add=TRUE)

## ----message=FALSE, warning=FALSE----------------------------------------
v.weights <- compute_weights(river, grid, "area", aoi=basin, basins = voronoi, riverID = "ID")

## ------------------------------------------------------------------------
plot(v.weights[[2]][,"weights"])

## ----message=FALSE, warning=FALSE----------------------------------------
plot(grid[,"gridID"], reset=FALSE)
plot(st_geometry(river), add=TRUE)

## ----message=FALSE, warning=FALSE----------------------------------------
l.weights <- compute_weights(river, grid, "length", aoi=basin, riverID = "ID")

## ----message=FALSE, warning=FALSE----------------------------------------
v.runoff <- compute_segment_runoff(v.weights)
l.runoff <- compute_segment_runoff(l.weights)

v.runoff
plot(v.runoff[,"TS9"], reset=FALSE)
plot(st_geometry(st_cast(grid, "LINESTRING")), add=TRUE)
plot(l.runoff[,"TS9"], reset=FALSE)
plot(st_geometry(st_cast(grid, "LINESTRING")), add=TRUE)

## ----message=FALSE, warning=FALSE----------------------------------------
v.flow <- accumulate_flow(v.runoff)
l.flow <- accumulate_flow(l.runoff)

plot(v.flow[,"TS9"])
plot(l.flow[,"TS9"])

