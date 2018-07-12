#' Split river segments at runoff unit (polygon) boundaries.
#' 
#' Splits an 'sf' linestring object at the boundaries of runoff units (polygons). 
#'
#' @inheritParams compute_weights
#'
#' @return Returns an 'sf' linestring object which has been split at the polygon (grid) boundaries with
#' attributes:
#' \itemize{
#'   \item \emph{ID}. Unique ID of the split river segments.
#'   \item \emph{riverID}. ID of the original river segment prior to splitting.
#'   \item \emph{gridID}. ID of the runoff unit split river segment is contained in.  
#' }
#' 
#' @examples 
#' \dontrun{
#' library(raster)
#' library(hydrostreamer)
#' 
#' # load data
#' data(river)
#' data(basin)
#' runoff <- brick(system.file("extdata", "runoff.tif", package = "hydrostreamer"))
#' 
#' # create HSgrid
#' grid <- polygrid_timeseries(grid, aoi=basin)
#' 
#' splitriver <- split_river_with_grid(river, grid, riverID="ID")
#' }
#' 
#' @export
split_river_with_grid <- function(river, grid, riverID = "riverID") {
    
    gridID <- NULL
    ID <- NULL
    
    #inspect input
    accepted <- c("LINESTRING", "MULTILINESTRING", "GEOMETRY")
    if(!any(class(river) %in% accepted) && !any(class(river) == "sf")) {
        stop("river input should be sf class LINESTRING or MULTILINESTRING")
    }
    if(!any(class(grid) == "HSgrid")) {
        stop("grid input should be of class HSgrid, obtained with function polygrid_timeseries()")
    }
    
    grid <- dplyr::select(grid, gridID)
    
    river <- suppressMessages(suppressWarnings(sf::st_intersection(river, grid)))
    river <- sf::st_cast(river, "LINESTRING")
    
    #add unique IDs
    if( any(names(river) == riverID) ) {
        river <- river %>% dplyr::rename_(riverID = riverID)
    }
    river$ID <- 1:NROW(river)
    river <- river %>% dplyr::select(ID, riverID, gridID, dplyr::everything())
    
    return(river)
}