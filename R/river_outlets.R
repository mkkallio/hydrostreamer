#' Identify locations from where to start catchment delineation.
#'
#' The function identifies the coordinates of raster cells which are just upstream from river confluence
#' points identified from a river network layer. The cell just upstream from confluence allows delineation
#' of segment specific catchments.
#' 
#' The function logic is such that it looks at the last node-pair of each segment in the river network. The angle
#' of the last node pair is computed, and the raster cell from where the last node-pair originated is identified. 
#' Note that the logic of the function requires that the river network is derived from the same raster than the 
#' drainage directions input to the function.
#' 
#' Note: Drainage directions must be in coded 1-128:
#' \itemize{
#'   \item{1}{East}
#'   \item{2}{Southeast}
#'   \item{4}{South}
#'   \item{8}{Southwest}
#'   \item{16}{West}
#'   \item{32}{Northwest}
#'   \item{64}{North}
#'   \item{128}{Northeast}
#' }
#'
#' @param drain.dir A RasterLayer object of drainage directions.
#' @inheritParams compute_weights
#'
#' @return Returns an 'sf' point object of identified raster cell centers. Includes all the attributes in the 
#' given \code{river} object.
#'
#' @examples 
#' \dontrun{
#' # to be added
#' }
#'  
#' @export
river_outlets <- function(river, drain.dir) {
    
    if(!any(class(river) == "sf")) {
        stop("river input should be sf class LINESTRING")
    }
    
    # shorten rivers the last node pair.
    p4s <- sf::st_crs(river)
    for(line in 1:NROW(river)) {
        coords <- sf::st_coordinates(river[line,])
        len <- NROW(coords)
        coords <- coords[(len-1):len,1:2]
        if(line == 1) {
            p <- sf::st_point(coords[2,]) %>% sf::st_sfc()
            b <- geosphere::bearing(coords)
        } else {
            p2 <- sf::st_point(coords[2,]) %>% sf::st_sfc()
            p <- c(p, p2)
            b2 <- geosphere::bearing(coords)
            b <- c(b, b2)
        }
    }
    b <- b[!is.na(b)]
    b <- b+180
    p <- sf::st_set_crs(p, p4s)
    
    # cells of interest
    rp <- raster::cellFromXY(drain.dir, sf::st_coordinates(p))
    nc <- raster::ncol(drain.dir)
    
    pointcoords <- data.frame(row = NULL, col = NULL)
    for (point in 1:length(rp)) {
        prc <- raster::rowColFromCell(drain.dir, rp[point]) %>% as.data.frame()
        
        cell <- rp[point]
        if(!is.na(cell)) {
            bearing <- b[point]
            
            prep <- data.frame(row=0, col=0)
            if (bearing > -5 && bearing < 5) {
                prep$row <- prc$row-1
                prep$col <- prc$col
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
            
            if (bearing > 350 && bearing < 370) {
                prep$row <- prc$row-1
                prep$col <- prc$col
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
            
            if (bearing > 40 && bearing < 50) {
                prep$row <- prc$row-1
                prep$col <- prc$col+1
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
            
            if (bearing > 80 && bearing < 100) {
                prep$row <- prc$row
                prep$col <- prc$col+1
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
            
            if (bearing > 125 && bearing < 145) {
                prep$row <- prc$row+1
                prep$col <- prc$col+1
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
            
            if (bearing > 170 && bearing < 190) {
                prep$row <- prc$row+1
                prep$col <- prc$col
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
            
            
            if (bearing > 215 && bearing < 235) {
                prep$row <- prc$row+1
                prep$col <- prc$col-1
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
            
            if (bearing > 260 && bearing < 280) {
                prep$row <- prc$row
                prep$col <- prc$col-1
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
            
            if (bearing > 305 && bearing < 325) {
                prep$row <- prc$row-1
                prep$col <- prc$col-1
                cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
                coords <- raster::xyFromCell(drain.dir, cell)
                pointcoords <- rbind(pointcoords, coords)
            }
        } else {
            coords <- c(NA,NA)
            pointcoords <- rbind(pointcoords, coords)
        }
        
    }
    
    #pointcoords <- matrix(pointcoords, ncol=2)
    points <- sf::st_multipoint(as.matrix(pointcoords)) %>%
        sf::st_sfc() %>%
        sf::st_cast("POINT") %>%
        sf::st_set_crs(p4s)
    
    sf::st_geometry(river) <- points
    empty <- sf::st_dimension(river)
    river <- river[!is.na(empty),]
    
    return(river)
    
}
