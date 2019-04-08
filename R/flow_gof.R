#' Computed goodness-of-fit statistics for discharge predictions
#' against observed streamflow
#' 
#' Computed goodness-of-fit statistics using \code{\link[hydroGOF]{gof}}
#' for all the timeseries in the input \code{HS} object. 
#' 
#' By default, goodness-of-fit measures are computed for all timeseries in 
#' the input data. 
#' 
#' @param HS A \code{HS} object with column \code{discharge_ts}
#' 
#' @return Returns a list with gof-statistics for all the observations in
#'  every discharge prediction in \code{HS}.
#' 
#' @export
flow_gof <- function(HS) {
    
    rowname <- NULL
    Prediction <- NULL
    Station <- NULL
    if(!hasName(HS, "observation_ts")) {
        stop("Observation timeseries missing. Please add one using
             add_observations()")
    }
    if(!hasName(HS, "discharge_ts")) {
        stop("discharge timeseries missing. Please compute it using
              accumulate_runoff()")
    }
 
    riverIDs <- lapply(HS$observation_ts, is.null)
    riverIDs <- which(!unlist(riverIDs))
    
    output <- list()
    for(stat in seq_along(riverIDs)) {
        data <- HS$discharge_ts[[ riverIDs[stat] ]]
        data <- dplyr::left_join(data, 
                          HS$observation_ts[[ riverIDs[stat] ]],
                          by = "Date")

        for(pred in 2:(ncol(data)-1)) {
            if(pred == 2) {
                gofs <- hydroGOF::gof(data[,pred],
                                      data[,"observations"], na.rm = TRUE)
            } else {
                gofs <- cbind(gofs,
                              hydroGOF::gof(data[,pred],
                                            data[,"observations"], na.rm = TRUE)) 
            }
             
        }
        gofs <- gofs %>% 
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column() %>%
            tibble::as_tibble() %>%
            dplyr::rename(Prediction = rowname) %>%
            dplyr::mutate(Station = HS$observation_station[ riverIDs[stat] ]) %>%
            dplyr::select(Prediction, Station, dplyr::everything())

            
        output[[stat]] <- gofs
    }
    
    output <- do.call("rbind", output)
    
    return(output)
}



