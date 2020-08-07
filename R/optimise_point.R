#' Computes optimal estimate at a specific river segment(s)
#' 
#' Function performs data assimilation by combining timeseries of downscaled 
#' discharge estimates against observed streamflow timeseries at all river 
#' segments with observations.
#' 
#' Optimisation can be performed either using ordinary least squares (OLS), 
#' Constrained Least Squares (CLS; coefficients positive, add to unity), 
#' Non-negative Least Squares (NNLS; coefficients positive), Least Squares 
#' without intercept (GRA), Least squares with no intercept, and coefficients
#' sum to unity (GRB), Bates-Granger (BG), Standard Eigenvector (EIG1), 
#' Bias-corrected Eigenvector (EIG2) or selecting the best performing ensemble 
#' member (best). 
#' 
#' Alternatively, R's \code{\link[stats]{optim}} can be used. In 
#' that case, \code{optim_method} should be a function which \code{optim} should
#' attempt to optimise. The function should accept three inputs: \code{par}, 
#' \code{obs} and \code{pred}. \code{par} is the vector of coefficients 
#' \code{optim} optimises, obs is the observation timeseries, and pred is a
#' matrix of inputs to be optimised. Additional arguments passed to \code{optim}
#' can also be defined. 
#' 
#' @param HS An \code{HS} object with observation_ts and discharge_ts
#' @param optim_method A character object giving the optimisation method to be 
#'   used, or a function to be passed to  \code{\link[stats]{optim}}. 
#'   See details.
#' @param combination Whether to do the forecast combination for the entire
#'   timeseries, or each month of the year individually, or for full calendar 
#'   years. Accepts \code{"timeseries"}, \code{"ts"}, or \code{"monthly"}, 
#'   \code{"mon"}, or \code{"annual"}, \code{"ann"}.
#' @param sampling How to sample training and testing periods. \code{"series"}
#'   for training serially from beginning, or \code{"random"} for a random
#'   sample for both training and testing periods. 
#' @param train The share of timeseries used for training period. 
#' @param ... parameters passed to \code{\link[stats]{optim}}, if optim_method
#'   input is a function.
#' 
#' @return Returns an object of class \code{HSoptim}, which is a list of
#' results for each observation station. Each list ielement contains
#'   \itemize{
#'     \item riverID
#'     \item Method: Model averaging method used.
#'     \item Weights: Vector of optimised model averaging weights. 
#'     \item Intercept: Intercept from the combination. \code{NA}, if not
#'       applicable to the method.
#'     \item Optimised_ts: A \code{tibble} consisting of date, observation and
#'       optimised timeseries.
#'     \item Goodness_of_fit. Goodness of fit of the forecast combination
#'       obtained using \code{\link[hydroGOF]{gof}}.
#' }
#' @export
optimise_point <- function(HS, 
                           optim_method="CLS",
                           combination = "ts",
                           sampling = "random",
                           train = 0.5,
                           ...) {
    
    warned_overfit <- FALSE
    warned_train <- FALSE
    bias_correction <- FALSE # disabled currently, likely to be removed
    log <- FALSE  # disabled currently, likely to be removed 
    
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
        
        if(nrow(flow) == 0) {
            message("Skipping station ", stat_names[rID], " for missing ",
                    "observation data.")
            next
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
                                               warned_train,
                                               ...)
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
                                            warned_train,
                                            ...)
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
                                           warned_train,
                                           ...)
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

