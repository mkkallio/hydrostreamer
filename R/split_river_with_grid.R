#' Split river segments at runoff unit (polygon) boundaries.
#' 
#' Splits an 'sf' linestring object at the boundaries of 
#' runoff units (polygons). 
#'
#' @param zoneID Name of the column in \code{HS} with unique IDs.
#' @inheritParams compute_HSweights
#' 
#' @return Returns an 'sf' linestring object which has been split at 
#'   the polygon (grid) boundaries with attributes (columns):
#'   \itemize{
#'     \item \emph{ID}. Unique ID of the split river segments.
#'     \item \emph{riverID}. ID of the original river segment prior 
#'       to splitting.
#'     \item \emph{zoneID}. ID of the runoff unit split river segment 
#'       is contained in. 
#'     \item Other columns inherited from \code{river}.
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
#' runoff <- brick(system.file("extdata", "runoff.tif", 
#'                             package = "hydrostreamer"))
#' 
#' # create HS
#' grid <- raster_to_HS(grid, aoi=basin)
#' 
#' splitriver <- split_river_with_grid(river, grid, 
#'                                     riverID="ID")
#' }
#' 
#' @export
split_river_with_grid <- function(river, 
                                  HS, 
                                  riverID = "riverID", 
                                  zoneID = "zoneID") {
    
    ID <- NULL
    
    #if("HS" %in% class(HS)) HS <- HS$grid
    grid <- HS %>% dplyr::select_(zoneID)
    
    river <- suppressMessages(suppressWarnings(sf::st_intersection(river, 
                                                                   grid)))
    
    #add unique IDs
    if( any(names(river) == riverID) ) {
        river <- river %>% dplyr::rename_(riverID = riverID)
    }
    river$ID <- 1:NROW(river)
    river <- river %>% dplyr::select(ID, riverID, zoneID, dplyr::everything())
    
    river <- tibble::as_tibble(river) %>%
        sf::st_as_sf()
    
    return(river)
}
