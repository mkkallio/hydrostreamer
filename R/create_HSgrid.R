#' Constructs a \code{HS} object from given input
#' 
#' Creates a \code{HS} from input where \code{grid} is a polygon grid
#' representing the spatialunits of runoff, and /code{runoff} is a data frame
#' containing column 'Date', and where column names match the grid IDs. 
#' 
#' @param grid An \code{sf POLYGON} object
#' @param runoff a table of runoff where rows are timesteps and columns
#'  correspond to specific polygons in \code{grid}.
#' @param zoneID Column name with unique IDs in \code{grid}.
#' @param name Name of the runoff timeseries. If multiple runoff timeseries
#'  are added to the same \code{HS} object using \code{\link{add_HS}}, 
#'  each timeseries must have a unique name, or else they are replaced.
#'  @inheritParams raster_to_HS
#'  
#' @return Returns a \code{HS} object with columns
#'   \itemize{
#'     \item zoneID: unique ID
#'     \item area: area in square meters.
#'     \item n_ts: number of runoff timeseries in the object.
#'     \item runoff_ts: a list column containing a \code{tsibble} of
#'       runoff timeseries.
#'   }   
#' 
#' @export
create_HS <- function(zones, 
                      runoff, 
                      unit,
                      zoneID = "zoneID", 
                      name="runoff_1") {
    
    Date <- NULL
    
    # --------------------------------------------------------------------------
    # TEST INPUT
    
    test <- "Date" %in% colnames(runoff)
    if(!test) stop("runoff input must have a 'Date' column.")
    
    test <- zoneID %in% colnames(zones)
    if(!test) stop("zones input must have a 'zoneID' column.")
    
    
    zones$zoneID <- dplyr::pull(zones, zoneID) 
    test <- length(zones$zoneID) == length(unique(zones$zoneID))
    if(!test) stop ("'zoneID' column contains duplicates")
    
    test <- all(zones$zoneID %in% colnames(runoff))
    if(!test) stop("some zoneID's missing from the column names in runoff")
    
    test <- "area" %in% colnames(zones)
    areas <- units::set_units(sf::st_area(zones), "km^2") 
    
    if(!test) zones <- tibble::add_column(zones, 
                                          area = areas)
    
    test <- unit != "mm/s"
    if(test) convert <- TRUE else convert <- FALSE
    
    
    #---------------------------------------------------------------------------
    # create a list column
    listc <- list()
    runoff <- tsibble::as_tsibble(runoff, index = Date) %>%
        dplyr::select("Date", dplyr::everything())
    
    for(i in 2:ncol(runoff)) {
        temp <- runoff[,c(1,i)]
        colnames(temp) <- c("Date", name)
        if(convert) {
            temp[,2] <- convert_unit(dplyr::pull(temp,2), unit, "mm/s", "m^3/s")
        } else {
            temp[,2] <- units::set_units(dplyr::pull(temp,2), 
                                         unit, mode="standard")
        }
        listc[[colnames(runoff)[i]]] <- temp
    }
    listc <- listc[order(names(listc))]
    n_ts <- lapply(listc, ncol) %>% unlist()
    
    # --------------------------------------------------------------------------
    # create output
    
    zones <- zones %>% dplyr::arrange(zoneID)
    
    output <- zones %>% tibble::add_column(n_ts = n_ts-1,
                                           runoff_ts = listc) %>%
        tibble::as_tibble(index = "Date") %>%
        sf::st_as_sf() 
    
    output <- reorder_cols(output)
    output <- assign_class(output, c("HS"))

    output <- mod_HS_attributes(output)
    return(output)
}
