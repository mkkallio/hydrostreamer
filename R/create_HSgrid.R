#' Constructs a \code{HSgrid} object from given input
#' 
#' Creates a \code{HSgrid} from input where grid is a polygon grid
#' representing the units of runoff. It must include a column 'gridID'
#' with unique IDs. Runoff must contain column 'Date', and value for 
#' runoff in columns, where the column name is the ID of the corresponding 
#' polygon in grid. 
#' 
#' @param grid An \code{sf POLYGON} object
#' @param runoff a table of runoff where rows are timesteps and columns
#'  correspond to specific polygons in \code{grid}.
#' @param name Name of the runoff timeseries. If multiple runoff timeseries
#'  are added to the same \code{HSgrid} object using \code{\link{add_HSgrid}}, 
#'  each timeseries must have a unique name.
#'  
#' @return Returns a \code{HSgrid} object which is a list of:
#'   \itemize{
#'     \item grid: polygon grid with areal units of runoff
#'     \item runoff: a list of runoff values for each polygon.
#'   }  
#' 
#' @export
create_HSgrid <- function(grid, runoff, name="runoff") {
    
    test <- "Date" %in% colnames(runoff)
    if(!test) stop("runoff input must have a 'Date' column.")
    
    test <- "gridID" %in% colnames(grid)
    if(!test) stop("grid input must have a 'gridID' column.")
    
    test <- length(grid$gridID) == length(unique(grid$gridID))
    if(!test) stop ("'gridID' column contains duplicates")
    
    test <- all(grid$gridID %in% colnames(runoff))
    if(!test) stop("some gridID's missing from the column names in runoff")
    
    test <- "area_m2" %in% colnames(grid)
    if(!test) grid <- tibble::add_column(grid, area_m2 = sf::st_area(grid)) 
    
    output <- list(grid = grid,
                   runoff = list(runoff))
    names(output$runoff) <- name
    class(output) <- "HSgrid"
}