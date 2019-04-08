#' Constructs a \code{HSgrid} object from given input
#' 
#' Creates a \code{HSgrid} from input where \code{grid} is a polygon grid
#' representing the spatialunits of runoff, and /code{runoff} is a data frame
#' containing column 'Date', and where column names match the grid IDs. 
#' 
#' @param grid An \code{sf POLYGON} object
#' @param runoff a table of runoff where rows are timesteps and columns
#'  correspond to specific polygons in \code{grid}.
#' @param gridID Column name with unique IDs in \code{grid}.
#' @param name Name of the runoff timeseries. If multiple runoff timeseries
#'  are added to the same \code{HSgrid} object using \code{\link{add_HSgrid}}, 
#'  each timeseries must have a unique name, or else they are replaced.
#'  
#' @return Returns a \code{HSgrid} object with columns
#'   \itemize{
#'     \item gridID: unique ID
#'     \item area_m2: area in square meters.
#'     \item n_ts: number of runoff timeseries in the object.
#'     \item runoff_ts: a list column containing a \code{tsibble} of
#'       runoff timeseries.
#'   }   
#' 
#' @export
create_HSgrid <- function(grid, 
                          runoff, 
                          gridID = "gridID", 
                          name="runoff_1") {
    
    Date <- NULL
    
    # Test input
    
    test <- "Date" %in% colnames(runoff)
    if(!test) stop("runoff input must have a 'Date' column.")
    
    test <- gridID %in% colnames(grid)
    if(!test) stop("grid input must have a 'gridID' column.")
    
    grid$gridID <- dplyr::pull(grid, gridID) 
    
    test <- all(!duplicated(grid$gridID))
    
    test <- length(grid$gridID) == length(unique(grid$gridID))
    if(!test) stop ("'gridID' column contains duplicates")
    
    test <- all(grid$gridID %in% colnames(runoff))
    if(!test) stop("some gridID's missing from the column names in runoff")
    
    test <- "area_m2" %in% colnames(grid)
    if(!test) grid <- tibble::add_column(grid, area_m2 = sf::st_area(grid)) 
    
    ######################
    # create a list column
    listc <- list()
    runoff <- tsibble::as_tsibble(runoff, index = Date) %>%
        dplyr::select("Date", dplyr::everything())
    
    for(i in 2:ncol(runoff)) {
        temp <- runoff[,c(1,i)]
        colnames(temp) <- c("Date", name)
        listc[[colnames(runoff)[i]]] <- temp
    }
    listc <- listc[order(names(listc))]
    n_ts <- lapply(listc, ncol) %>% unlist()
    
    grid <- grid %>% dplyr::arrange(gridID)
    
    output <- grid %>% tibble::add_column(n_ts = n_ts-1,
                                          runoff_ts = listc) %>%
        tibble::as_tibble(index = "Date") %>%
        sf::st_as_sf() 
    
    output <- reorder_cols(output)
    output <- assign_class(output, c("HSgrid", "HS"))
    return(output)
}
