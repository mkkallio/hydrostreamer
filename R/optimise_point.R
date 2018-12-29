#' Computes optimal estimate from multiple input discharge timeseries
#' 
#' Function performs timeseries optimisation of discharge estimates against
#' observed streamflow timeseries at specific river segments.
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
#' If bias correction is set to TRUE, a constant bias correction is applied
#' to the entire timeseries so that mean error in the training period is 0.
#' 
#' @param HSflow An \code{HSflow} object
#' @param HSobs An \code{HSobs} object containing observations at the
#'   locations to optimise.
#' @param optim_method Method used to optimise. Default uses constrained
#'   linear regression. See details.
#' @param train The share of timeseries used for training period. 
#' @param bias_correction Apply (constant) bias correction. Defaults to FALSE.
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
optimise_point <- function(HSflow, 
                           HSobs, 
                           optim_method="CLS",
                           train=0.5, 
                           bias_correction=FALSE) {
    riverIDs <- HSobs$riverIDs
    #nstat <- length(HSobs$riverIDs)
    
    nflow <- length(HSflow$discharge)

    if (is.null(HSobs$Observations$Date)) {
        dates <- HSobs$Observations$Month
        usedates <- "Month"
    } else {
        dates <- HSobs$Observations$Date
        usedates <- "Date"
    }
    
    fnames <- names(HSflow$discharge)
    stat_names <- colnames(HSobs$Observation[,-1])
    
    # do forecast combination for each station
    combs <- list()
    statobs <- list()
    statdates <- list()
    traintest <- list()
    for (rID in seq_along(riverIDs)) {
        #flow <- list()
       
        flow <- HSobs$Observations[,c(1,rID+1)]
        for(pred in seq_along(HSflow$discharge)){
            ind <- which(colnames(HSflow$discharge[[pred]]) == riverIDs[rID])
            temp <- HSflow$discharge[[pred]][,c(1,ind)]
            flow <- dplyr::left_join(flow, temp, by=c("Date"))
        }
        colnames(flow) <- c("Date", "Obs", fnames)
        colremove <- apply(flow,2,FUN=function(x) all(is.na(x)))
        flow <- flow[,!colremove]
        flow <- flow[complete.cases(flow),]
        
        if(nrow(flow) < 12) {
            warning("At least one of optimized stations have not enough (10) matching 
                    observation and prediction dates.")
          next
        }
        
        train_ <- 1:(round(nrow(flow)*train, 0))
        test_ <- (max(train_)+1):nrow(flow)
        
        fcast <- suppressMessages(ForecastComb::foreccomb(flow$Obs[train_], 
                                                 flow[train_,-c(1,2)], 
                                                 flow$Obs[test_], 
                                                 flow[test_,-c(1,2)]))
        
        if(optim_method == "factorCLS") {
            # This is the function ForecastComb::comb_CLS() in a factorized form 
            # because the original unfactorized ForecastComb function results often 
            # in no solutions error in solve.QP(). The fix is sourced from 
            # StackOverflow: https://stackoverflow.com/a/28388394
            combs[[rID]] <- hydrostreamer:::forecastcomb_comb_CLS(fcast) 
        } else if (optim_method == "CLS") {
            combs[[rID]] <- ForecastComb::comb_CLS(fcast)
        } else if (optim_method == "OLS") {
            combs[[rID]] <- ForecastComb::comb_OLS(fcast)
        } else if (optim_method == "BG") {
            combs[[rID]] <- ForecastComb::comb_BG(fcast)
        } else if (optim_method == "EIG1") {
            combs[[rID]] <- ForecastComb::comb_EIG1(fcast)
        } else if (optim_method == "EIG2") {
            combs[[rID]] <- ForecastComb::comb_EIG2(fcast)
        } else if (optim_method == "EIG3") {
            combs[[rID]] <- ForecastComb::comb_EIG3(fcast)
        } else if (optim_method == "EIG4") {
            combs[[rID]] <- ForecastComb::comb_EIG4(fcast)
        } else if (optim_method == "InvW") {
            combs[[rID]] <- ForecastComb::comb_InvW(fcast)
        } else if (optim_method == "LAD") {
            combs[[rID]] <- ForecastComb::comb_LAD(fcast)
        } else if (optim_method == "MED") {
            combs[[rID]] <- ForecastComb::comb_MED(fcast)
        } else if (optim_method == "NG") {
            combs[[rID]] <- ForecastComb::comb_NG(fcast)
        } else if (optim_method == "SA") {
            combs[[rID]] <- ForecastComb::comb_SA(fcast)
        } else if (optim_method == "TA") {
            combs[[rID]] <- ForecastComb::comb_TA(fcast)
        } else if (optim_method == "WA") {
            combs[[rID]] <- ForecastComb::comb_WA(fcast)
        } else if (optim_method == "auto") {
            combs[[rID]] <- ForecastComb::auto_combine(fcast)
        } 
        
        statobs[[rID]] <- flow[,1:2]
        traintest[[rID]] <- max(train_)
        
    }
    
    # create output report
    output <- list()
    for(i in seq_along(combs)) {
        
        if (bias_correction) {
            bias<- combs[[i]]$Accuracy_Train[1]
            #bias_test <- combs[[i]]$Accuracy_Test[1]
            ts_train <- combs[[i]]$Fitted+bias
            ts_test <- combs[[i]]$Forecasts_Test+bias
        } else {
            ts_train <- combs[[i]]$Fitted
            ts_test <- combs[[i]]$Forecasts_Test
        }
        
        
        
        statdates <- statobs[[i]]$Date
        sobs <- statobs[[i]]$Obs

        # combining by OLS may result in NA for forecasts if the timeseries 
        # is too short. In that case, hydroGOF::gof() fails. Here we make sure 
        # that if there are NA's in either of forecast timeseries, gof() will 
        # not be used.
        if(any(is.na(combs[[i]]$Forecasts_Train)) || any(is.na(combs[[i]]$Forecasts_Test))) {
            gofs <- NA
        } else {
            
            gof_train <- hydroGOF::gof(ts_train, sobs[1:traintest[[i]]])
            gof_test <- hydroGOF::gof(ts_test, sobs[(traintest[[i]]+1):length(sobs)])
            gof_all <- hydroGOF::gof(c(ts_train,ts_test), sobs)
            gofs <- data.frame(Train_period = gof_train, 
                               Test_period = gof_test, 
                               Train_and_test_periods = gof_all)   
        }
        residuals_train <- ts_train-sobs[1:traintest[[i]]]
        residuals_test  <- ts_test-sobs[(traintest[[i]]+1):length(sobs)]
        
        
        weights <- combs[[i]]$Weights
        names(weights) <- combs[[i]]$Models
        if(is.null(combs[[i]]$Intercept)) intercept <- NA else intercept <- combs[[i]]$Intercept 
        
        
        output[[ stat_names[i] ]] <- list(riverID = riverIDs[i],
                                          Observations = data.frame(Date = as.Date(statdates), 
                                                                Observations = sobs),
                                          Forecast_train = data.frame(Date = as.Date(statdates[1:traintest[[i]]]), 
                                                                 Forecast = ts_train,
                                                                 Residuals = residuals_train),
                                          Forecast_test = data.frame(Date = as.Date(statdates[(traintest[[i]]+1):length(statdates)]),
                                                                Forecast = ts_test,
                                                                Residuals = residuals_test),
                                          Forecast_train_test = data.frame(Date = as.Date(statdates), 
                                                                      Forecast = c(ts_train,ts_test),
                                                                      Residuals = c(residuals_train,residuals_test)),
                                          Method = combs[[i]]$Method,
                                          Forecast_weights = weights,
                                          Intercept = intercept,
                                          Goodness_of_fit = gofs)
    }
    
    class(output) <- "HSoptim"
  
    return(output)
}
