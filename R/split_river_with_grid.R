#' Split river segments at runoff unit (polygon) boundaries.
#' 
#' Splits an 'sf' linestring object at the boundaries of 
#' runoff units (polygons). 
#'
#' @param gridID Name of the column in \code{HSgrid} with unique IDs.
#' @inheritParams compute_HSweights
#' 
#' @return Returns an 'sf' linestring object which has been split at 
#'   the polygon (grid) boundaries with attributes (columns):
#'   \itemize{
#'     \item \emph{ID}. Unique ID of the split river segments.
#'     \item \emph{riverID}. ID of the original river segment prior 
#'       to splitting.
#'     \item \emph{gridID}. ID of the runoff unit split river segment 
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
#' # create HSgrid
#' grid <- raster_to_HSgrid(grid, aoi=basin)
#' 
#' splitriver <- split_river_with_grid(river, grid, 
#'                                     riverID="ID")
#' }
#' 
#' @export
split_river_with_grid <- function(river, 
                                  HSgrid, 
                                  riverID = "riverID", 
                                  gridID = "gridID") {
    
    ID <- NULL
    
    #if("HSgrid" %in% class(HSgrid)) HSgrid <- HSgrid$grid
    grid <- HSgrid %>% dplyr::select_(gridID)
    
    river <- suppressMessages(suppressWarnings(sf::st_intersection(river, 
                                                                   grid)))
    
    #add unique IDs
    if( any(names(river) == riverID) ) {
        river <- river %>% dplyr::rename_(riverID = riverID)
    }
    river$ID <- 1:NROW(river)
    river <- river %>% dplyr::select(ID, riverID, gridID, dplyr::everything())
    
    river <- tibble::as_tibble(river) %>%
        sf::st_as_sf()
    
    return(river)
}
