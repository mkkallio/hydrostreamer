#' Constructs a \code{HS} object from given input
#' 
#' Creates a \code{HS} from input where \code{grid} is a polygon grid
#' representing the spatial units of runoff, and /code{runoff} is a data.frame
#' containing column 'Date', and where column names match the grid IDs. 
#' 
#' @param zones An \code{sf POLYGON} object
#' @param runoff a table of runoff where rows are timesteps and columns
#'  correspond to specific polygons in \code{grid}.
#' @param unit Unit of the runoff timeseries
#' @param zoneID Column name with unique IDs in \code{grid}.
#' @param name Name of the runoff timeseries. If multiple runoff timeseries
#'  are added to the same \code{HS} object using \code{\link{add_HS}}, 
#'  each timeseries must have a unique name, or else they are replaced.
#' @param handle_negative How to handle egative runoff values (not supported in
#'   hydrostreamer): "zero" sets negative runoff to 0, "error" stops execution
#'   and returns an error.
#'  @inheritParams raster_to_HS
#'  
#' @return Returns a \code{HS} object with columns
#'   \itemize{
#'     \item zoneID: unique ID
#'     \item runoff_ts: a list column containing the runoff timeseries.
#'   }   
#' 
#' @export
create_HS <- function(zones, 
                      runoff, 
                      unit,
                      zoneID = "zoneID", 
                      name = "runoff_1",
                      handle_negative = "zero") {
    
    Date <- NULL
    
    # --------------------------------------------------------------------------
    # TEST INPUT
    
    test <- "Date" %in% colnames(runoff)
    if(!test) stop("runoff input must have a column named 'Date'.")
    date_ind <- which(names(runoff) == "Date")
    
    test <- zoneID %in% colnames(zones)
    if(!test) stop("zoneID column ", zoneID, " not found in zones")
    
    
    zones$zoneID <- dplyr::pull(zones, zoneID) 
    test <- length(zones$zoneID) == length(unique(zones$zoneID))
    if(!test) stop ("'zoneID' column contains duplicates. Please ensure ", 
                    "that each ID is unique.")
    
    test <- all(zones$zoneID %in% colnames(runoff))
    if(!test) stop("some zoneID's missing from the column names in runoff")
    
    test <- unit != "mm/s"
    if(test) convert <- TRUE else convert <- FALSE
    
    test <- any(runoff[,-date_ind] < 0, na.rm = TRUE)
    if(test) {
        if(handle_negative == "zero") {
            runoff[runoff < 0] <- 0
            warning("Negative runoff values set to 0.")
        } else if(handle_negative == "error") {
            stop("Negative runoff values not permitted.")
        } else {
            stop("Don't know how to process handle_negative = ", handle_negative)
        }
    }
    
    #---------------------------------------------------------------------------
    # create a list column
    listc <- list()
    runoff <- tibble::as_tibble(runoff, .name_repair = "minimal") %>%
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
    
    # --------------------------------------------------------------------------
    # create output

    output <- zones %>% 
        tibble::add_column(runoff_ts = listc) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        sf::st_as_sf() 
    
    output <- reorder_cols(output)
    output <- assign_class(output, c("HS"))

    output <- mod_HS_attributes(output)
    return(output)
}
