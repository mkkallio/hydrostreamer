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
split_river_with_grid <- function(river, 
                                  HS, 
                                  riverID = "riverID", 
                                  zoneID = "zoneID") {
    
    ID <- NULL
    geometry <- NULL
    
    grid <- dplyr::select(HS, zoneID = !!zoneID)
    
    river <- suppressMessages(suppressWarnings(sf::st_intersection(river, 
                                                                   grid)))
    
    river$line_length <- sf::st_length(river)
    river$line_length_corr <- river$line_length
    ### handle river segments at the boundaries
    gridint <- suppressMessages(
        suppressWarnings(
            sf::st_intersection(river, 
                                sf::st_geometry(
                                    sf::st_cast(
                                        sf::st_cast(grid, 
                                                    "MULTILINESTRING"),
                                        "LINESTRING")))))
    ind <- sf::st_is(gridint, "LINESTRING")
    if(sum(ind) != 0) {
        gridint <- gridint[ind,]
        gridint <- dplyr::distinct(gridint, !!riverID, geometry)
        ids <- unique(gridint$riverID)
        for(i in ids) {
            ind_riv <- river$riverID == i
            ind_grid <- gridint$riverID == i
            l <- sf::st_length(gridint[ind_grid,])
            river$line_length_corr[ind_riv] <- river$line_length[ind_riv] - l/2
        }
    }

    
    #add unique IDs
    river$ID <- 1:NROW(river)
    river <- river %>% dplyr::select(ID, riverID, zoneID, dplyr::everything())
    
    river <- tibble::as_tibble(river, .name_repair = "minimal") %>%
        sf::st_as_sf()
    
    return(river)
}
