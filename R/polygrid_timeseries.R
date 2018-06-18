#' Polygonizes a raster and adds cell values as attribute columns to an 'sf' polygon object.
#'
#' @param brick A Raster* object.
#' @param aoi Area of interest as an 'sf' polygon object, 'SpatialPolygons', or 'SpatialPolygonsDataFrame'.
#'   Optional, but recommended for all applications.
#' @param timesteps A logical vector with same length as there are layers in brick, or a numeric vector
#'   specifying the layers to process. Optional. If not given, will process all raster layers.
#'
#' @return Returns an 'sf' polygon object, with each polygon representing a raster cell, and which has been
#'   cropped to the area of interest. Columns are ID, area of the polygon in m^2, and each timestep as separate
#'   column. Output has class 'HSgrid'.
#' @export
#'
#'
polygrid_timeseries <- function(brick, aoi=NULL, timesteps = NULL) {
    
    # test for brick-class
    accepted <- c("RasterLayer", "RasterStack", "RasterBrick")
    if (!any(class(brick) %in% accepted)) stop("brick needs to be a Raster* object")
    
    # see if aoi exists, whether it is an 'sf' or 'sp', and crop
    if (!is.null(aoi)) {
        
        # if aoi is an 'sf' object, else if its an 'sp' object, else stop
        accepted <- c("sf", "sfc", "sfg")
        if( any(class(aoi) %in% accepted) ) {
            aoi <- as(aoi, "Spatial")
            brick <- raster::crop(brick, aoi, snap="out")
            aoi <- sf::st_as_sf(aoi)
        } else if ( grepl("Spatial", class(aoi), fixed=TRUE) ) {
            brick <- raster::crop(brick, aoi)
            aoi <- sf::st_as_sf(aoi)
        } else {
            stop("Input area of interest should be an object of class from either 'sf' or 'sp' packages")
        }
    }
    
    # are timesteps given? polygonize
    if (is.null(timesteps)) {
        grid <- raster::rasterToPolygons(brick)
    } else {
        grid <- raster::rasterToPolygons(brick[[timesteps]])
    }
    
    # change class to 'sf', intersect grid to aoi, if exists
    if(!is.null(aoi)) {
        grid <- suppressWarnings(suppressMessages(as(grid, "sf") %>%
                                                      sf::st_intersection(., sf::st_union(aoi))))
    }
    
    
    # add ID and area columns and move them in the beginning of data frame
    grid$gridID <- 1:NROW(grid)
    grid$area_m2 <- sf::st_area(grid)
    grid <- grid %>% dplyr::select(gridID, area_m2, dplyr::everything())
    
    # assign class
    class(grid) <- append(class(grid),"HSgrid")
    
    #return
    return(grid)
}
