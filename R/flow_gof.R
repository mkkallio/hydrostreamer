#' Computed goodness-of-fit statistics for discharge predictions
#' against observed streamflow
#' 
#' Computed goodness-of-fit statistics using \code{\link[hydroGOF]{gof}}
#' for all the timeseries in the input \code{HS} object. 
#' 
#' @param HS A \code{HS} object with column \code{discharge_ts}.
#' @param verbose Print progress indication or not.
#' 
#' @return Returns a tibble with gof-statistics for all the observations in
#'  every discharge prediction in \code{HS}.
#' 
#' @export
flow_gof <- function(HS, verbose = FALSE) {
    
    rowname <- NULL
    Prediction <- NULL
    Station <- NULL
    if(!hasName(HS, "observation_ts")) {
        stop("Observation timeseries missing. Please add one using ",
             " add_observations()")
    }
    if(!hasName(HS, "discharge_ts")) {
        stop("discharge timeseries missing. Please compute it using ",
              "accumulate_runoff()")
    }
 
    riverIDs <- lapply(HS$observation_ts, is.null)
    riverIDs <- which(!unlist(riverIDs))
    
    total <- c(0, sapply(1:length(riverIDs), 
                    function(x) ncol(HS$discharge_ts[[ riverIDs[x] ]])-1))
    output <- list()
    if(verbose) pb <- txtProgressBar(min = 0, max = sum(total), style = 3)
    for(stat in seq_along(riverIDs)) {
        
        data <- HS$discharge_ts[[ riverIDs[stat] ]]
        data <- dplyr::left_join(data, 
                          HS$observation_ts[[ riverIDs[stat] ]],
                          by = "Date")
        
        test <- all(is.na(data$observations))
        if(test) {
            message("No observations for the data period. Skipping station ",
                    HS$observation_station[[ riverIDs[stat] ]])
            next
        }

        gofs <- matrix(NA, ncol=20, nrow=(ncol(data)-2))
        colnames(gofs) <- c("ME", "MAE", "MSE", "RMSE", "NRMSE %", "PBIAS %",
                            "RSR", "rSD","NSE", "mNSE", "rNSE", "d", "md", "rd",
                            "cp", "r", "R2", "bR2", "KGE", "VE")
        rownames(gofs) <- colnames(data)[-c(1,ncol(data))]
        for(pred in 2:(ncol(data)-1)) {
            
            gofs[pred-1,] <- hydroGOF::gof(data[,pred],
                                         data[,"observations"], na.rm = TRUE) 
            if(verbose) setTxtProgressBar(pb, sum(total[1:stat])+pred)
        }
        
        gofs <- gofs %>% 
            #t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column() %>%
            tibble::as_tibble(.name_repair = "minimal") %>%
            dplyr::rename(Prediction = rowname) %>%
            dplyr::mutate(Station=HS$observation_station[ riverIDs[stat] ]) %>%
            dplyr::select(Prediction, Station, dplyr::everything())

            
        output[[stat]] <- gofs
    }
    if(verbose) close(pb)
    
    output <- do.call("bind_rows", output)
    
    return(output)
}



