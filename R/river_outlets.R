#' Identify locations from where to start catchment delineation.
#'
#' The function identifies the coordinates of raster cells which are just 
#' upstream from river confluence points identified from a river network layer. 
#' The cell just upstream from confluence allows delineation of segment specific
#' catchments.
#' 
#' The function logic is such that it looks at the last node-pair of each 
#' segment in the river network. The angle of the last node pair is computed, 
#' and the raster cell from where the last node-pair originated is identified. 
#' Note that the logic of the function requires that the river network is 
#' derived from the same raster than the drainage directions input to the 
#' function.
#' 
#' Note: Drainage directions must be coded in 1-128 as follows:
#' \itemize{
#'   \item{1}{:East}
#'   \item{2}{:Southeast}
#'   \item{4}{:South}
#'   \item{8}{:Southwest}
#'   \item{16}{:West}
#'   \item{32}{:Northwest}
#'   \item{64}{:North}
#'   \item{128}{:Northeast}
#' }
#'
#' @param river An 'sf' linestring feature representing a river network.
#' @param drain.dir A RasterLayer object of drainage directions.
#'
#' @return Returns an 'sf' point object of identified raster cell centers. 
#' Includes all the attributes in the given \code{river} object.
#'  
#' @export
river_outlets <- function(river, drain.dir) {
    
    if(!any(class(river) == "sf")) {
        stop("river input should be sf class LINESTRING")
    }
    
    p4s <- sf::st_crs(river)
    b <- vector("numeric", nrow(river))
    pointcoords <- matrix(NA, ncol=2, nrow=nrow(river))
    
    # compute bearing
    if( grepl("longlat", p4s[2], fixed=TRUE) ) {
        for(line in 1:NROW(river)) {
            coords <- sf::st_coordinates(river[line,])
            len <- NROW(coords)
            coords <- coords[(len-1):len,1:2]
            
            pointcoords[line,] <- coords[2,]
            
            b[line] <- geosphere::bearing(coords)[1]
        }
        b <- b+180
    } else {
        for(line in 1:NROW(river)) {
            coords <- sf::st_coordinates(river[line,])
            len <- NROW(coords)
            coords <- coords[(len-1):len,1:2]
            
            pointcoords[line,] <- coords[2,]
            
            dx <- coords[1,1]-coords[2,1]
            dy <- coords[1,2]-coords[2,2]
            angle <- 90 - (180/pi)*atan2(dy,dx)
            
            b[line] <- angle %% 360
        }
    }
    
    # cells of interest
    rp <- raster::cellFromXY(drain.dir, pointcoords)
    
    pointcoords <- matrix(NA, nrow=length(rp), ncol=2)
    for (point in 1:length(rp)) {
        prc <- raster::rowColFromCell(drain.dir, rp[point]) %>% as.data.frame()
        
        cell <- rp[point]
        if(!is.na(cell)) {
            bearing <- b[point]
            
            prep <- new_row_col(bearing, prc)
            
            cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
            coords <- raster::xyFromCell(drain.dir, cell)
            pointcoords[point,] <- coords
            
        } else {
            coords <- c(NA,NA)
            pointcoords[point,] <- coords
        }
        
    }
    points_in <- complete.cases(pointcoords)
    pointcoords <- pointcoords[points_in,]
    river <- river[points_in,]
    
    points <- sf::st_multipoint(pointcoords) %>%
        sf::st_sfc() %>%
        sf::st_cast("POINT") %>%
        sf::st_set_crs(p4s)
    
    sf::st_geometry(river) <- points
    empty <- sf::st_dimension(river)
    river <- river[!is.na(empty),]
    
    return(river)
    
}
