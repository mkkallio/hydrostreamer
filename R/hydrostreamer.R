#' hydrostreamer: A package for assigning runoff outputs from hydrological or land surface models to explicit river segments.
#'
#'hydrostreamer is an R package to downscale off-the-shelf runoff timeseries into explicitly represented
#'river network. Downscaling is done by the spatial relationship between a polygon (raster cell) with runoff information
#'and the river network. The cell value is divided among those river segments which intersect the cell in question using
#'weights based on catchment area within the cell, or by river segment properties. Simple river routing algorithm is also
#'provided to estimate discharge at each segment.
#'
#' @docType package
#' @name hydrostreamer
NULL
