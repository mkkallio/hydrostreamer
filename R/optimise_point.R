#' Computes optimal estimate at a specific river segment 
#' 
#' Function performs data assimilation by combining timeseries of downscaled 
#' discharge estimates against observed streamflow timeseries at all river 
#' segments with observations.
#' 
#' Optimisation of the input timeseries against observations makes use of 
#' \code{forecastComb} package, with three options currently supported;
#' \code{"CLS"} (constrained linear regression), \code{"OLS"} (ordinary
#' linear regression), and \code{"factorCLS"} (constrained linear regression
#' in factorized form). Default is constrained linear regression, which 
#' constraints the coefficients to add to unity, and all weights are between
#' 0, and 1. CLS also forces Intercept to 0. In some cases, CLS fails to 
#' find a unique solution. In those cases, you can try the factorized
#' CLS. 
#' 
#' If bias correction is set to TRUE, bias correction is applied
#' to the entire timeseries so that bias % in the training period is 0.
#' 
#' @param HS An \code{HS} object with observation_ts and discharge_ts
#' @param optim_method Method used to optimise. Default uses constrained
#'   linear regression. See details.
#' @param combination Whether to do the forecast combination for the entire
#'   timeseries, or each month of the year individually, or for full calendar 
#'   years. Accepts \code{"timeseries"}, \code{"ts"}, or \code{"monthly"}, 
#'   \code{"mon"}, or \code{"annual"}, \code{"ann"}.
#' @param sampling How to sample training and testing periods. \code{"series"}
#'   for training serially from beginning, or \code{"random"} for a random
#'   sample for both training and testing periods. 
#' @param train The share of timeseries used for training period. 
#' @param bias_correction Apply bias correction. Defaults to FALSE.
#' @param log Log-transform prediction and observation timeseries before 
#'   fitting. Defaults to \code{FALSE}.
#' 
#' @return Returns an object of class \code{HSoptim}, which is a list of
#' results from observation stations. Each list item contains:
#'   \itemize{
#'     \item Observations: Supllied observation timeseries.
#'     \item Forecast_train: Forecasted timeseries at training period.
#'     \item Forecast_test: Forecasted timeseries at testing period.
#'     \item Forecast_train_test: Forecasted timeseries with training and
#'       testing combined
#'     \item Method: Used method for forecast combination.
#'     \item Forecast_weights: Vector of weights - how the input discharge 
#'       timeseries were combined.
#'     \item Intercept: Intercept from the combination. \code{NA}, if not
#'       applicable to the Method.
#'     \item Goodness_of_fit. Goodness of fit of the forecast combination
#'       obtained using \code{\link[hydroGOF]{gof}}.
#' }
#' @export
optimise_point <- function(HS, 
                           optim_method="CLS",
                           combination = "ts",
                           sampling = "random",
                           train = 0.5, 
                           bias_correction = FALSE,
                           log = FALSE) {
    
    warned_overfit <- FALSE
    warned_train <- FALSE
    
    riverIDs <- lapply(HS$observation_ts, is.null)
    riverIDs <- which(!unlist(riverIDs))
    
    stat_names <- HS$observation_station[ riverIDs ]
    
    
    ##########################################
    # do forecast combination for each station
    ##########################################
    
    combs <- list()
    
    for (rID in seq_along(riverIDs)) {
        
        flow <- dplyr::left_join(HS$discharge_ts[[ riverIDs[rID] ]],
                                 HS$observation_ts[[ riverIDs[rID] ]],
                                 by="Date")
        
        flow <- flow[!is.na(flow$observations),]
        
        colremove <- apply(flow, 2, FUN=function(x) all(is.na(x)))
        if(any(colremove)) {
            flow <- flow[,names(colremove)[!colremove]]
        }
        
        #flow <- remove_collinear(flow)
        
        ############################################
        # Forecast combination entire timeseries or monthly or annually
        
        
        if(combination %in% c("timeseries", "ts")) {
            
            combs[[rID]] <- combine_timeseries(flow, 
                                               optim_method, 
                                               sampling,
                                               train,
                                               bias_correction,
                                               log,
                                               warned_overfit,
                                               warned_train)
            warned_overfit <- combs[[rID]]$warned_overfit
            warned_train <- combs[[rID]]$warned_train
            
        } else if(combination %in% c("monthly", "mon")) {
            
            combs[[rID]] <- combine_monthly(flow, 
                                            optim_method, 
                                            sampling,
                                            train,
                                            bias_correction,
                                            log,
                                            warned_overfit,
                                            warned_train)
            warned_overfit <- combs[[rID]]$warned_overfit
            warned_train <- combs[[rID]]$warned_train
            
        } else if(combination %in% c("annual", "ann")) {
            
            combs[[rID]] <- combine_annual(flow, 
                                           optim_method, 
                                           sampling,
                                           train,
                                           bias_correction,
                                           log,
                                           warned_overfit,
                                           warned_train)
            warned_overfit <- combs[[rID]]$warned_overfit
            warned_train <- combs[[rID]]$warned_train
        }
    }
    
    ######################
    # create output
    ######################
    
    output <- list()
    for(i in seq_along(combs)) {
        
        output[[ stat_names[i] ]] <- list(riverID = HS$riverID[riverIDs[i]],
                                  Method = combs[[i]]$Method,
                                  Weights = combs[[i]]$Weights,
                                  Intercept = combs[[i]]$Intercept,
                                  Bias_correction = combs[[i]]$Bias_correction,
                                  Optimized_ts = combs[[i]]$Optimized_ts,
                                  Goodness_of_fit = combs[[i]]$Goodness_of_fit)
    }
    
    output <- assign_class(output, "HSoptim")
    
    return(output)
}

