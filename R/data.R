#' Example river network
#'
#' A river network of 216 segments used in the vignette of hydrostreamer.
#'
#' @format An 'sf' LINESTRING object with one attribute.
#' \describe{
#'   \item{SEGMENT_ID}{unique ID}
#'   \item{geometry}{'sfc' geometry column}
#' }
"river"

#' Catchment areas for 41 river segments
#'
#' River segment specific catchment areas for 41 river segment in 
#' "example_rivers" data, also provided in the package.
#'
#' @format An 'sf' POLYGON object with no attributes.
#' \describe{
#'   \item{SEGMENT_ID}{unique ID}
#'   \item{geometry}{'sfc' geometry column}
#' }
"basins"

#' DEM for examples
#' 
#' A 1 by 1 degree ALOS World 3D DEM resampled to 0.05 degree resolution. 
#' Original DEM by JAXA. 
#' 
#' T. Tadono, H. Ishida, F. Oda, S. Naito, K. Minakawa, 
#' H. Iwamoto : Precise Global DEM Generation By ALOS PRISM, ISPRS Annals of the 
#' Photogrammetry, Remote Sensing and Spatial Information Sciences, Vol.II-4, 
#' pp.71-76, 2014. 
#' 
#' @format RasterBrick / GeoTIFF
"dem"

#' Runoff timeseries for examples
#' 
#' 1 by 1 degree runoff timeseries in the same area as the DEM. Runoff is sourced 
#' from the Linear Optimal Runoff Aggregate (LORA) at 0.5 degree resolution.
#' The unit of runoff is mm/s (kg/m2/s), and are provided with a monthly timestep.
#' 
#' Hobeichi, S., Abramowitz, G., Evans, J., and Beck, H. E.: Linear Optimal 
#' Runoff Aggregate (LORA): a global gridded synthesis runoff product, 
#' Hydrol. Earth Syst. Sci., 23, 851-870, https://doi.org/10.5194/hess-23-851-2019, 
#' 2019.
#'  
#' @format RasterLayer / GeoTIFF  
"runoff"