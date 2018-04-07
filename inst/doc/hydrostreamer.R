## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(sf)
library(raster)
library(hydrostreamer)

data(river)
data(basin)
runoff <- brick(system.file("extdata", "runoff.tif", package = "hydrostreamer"))


## ------------------------------------------------------------------------
plot(runoff[[1]])
plot(st_geometry(river), add=TRUE)
plot(st_geometry(basin), add=TRUE)

## ----message=FALSE, warning=FALSE----------------------------------------
grid <- polygrid_timeseries(runoff, aoi=basin)
names(grid)
plot(grid[,"area_m2"])

## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
voronoi <- river_voronoi(river, aoi=basin)

## ----message=FALSE, warning=FALSE----------------------------------------
plot(voronoi[,"riverID"])
plot(st_geometry(river), add=TRUE)

## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-------------
#  v.weights <- compute_weights(river, grid, "area", aoi=basin)

## ----message=TRUE, warning=TRUE, include=FALSE---------------------------
v.weights <- compute_weights(river, grid, "area", aoi=basin, basins = voronoi)

## ------------------------------------------------------------------------
plot(v.weights[[2]][,"weights"])

## ----message=FALSE, warning=FALSE----------------------------------------
plot(grid[,"ID"])
plot(st_geometry(river), add=TRUE)

## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
l.weights <- compute_weights(river, grid, "length", aoi=basin)

## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
v.runoff <- compute_segment_runoff(v.weights)
l.runoff <- compute_segment_runoff(l.weights)

## ------------------------------------------------------------------------
v.runoff

## ----message=FALSE, warning=FALSE----------------------------------------
plot(v.runoff[,"TS9"])
plot(st_geometry(st_cast(grid, "LINESTRING")), add=TRUE)
plot(l.runoff[,"TS9"])
plot(st_geometry(st_cast(grid, "LINESTRING")), add=TRUE)

## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
v.flow <- accumulate_flow(v.runoff)
l.flow <- accumulate_flow(l.runoff)

## ----message=FALSE, warning=FALSE----------------------------------------
plot(v.flow[,"TS9"])
plot(l.flow[,"TS9"])

