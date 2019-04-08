bias_correct <- function(combs) {
    
    # get bias
    bias <- (100+(-combs$Goodness_of_fit[6, 1])) / 100
    
    combs[["Bias_correction"]] <- -combs$Goodness_of_fit[6, 1]

    # update timeseries with bias
    combs$Optimized_ts$Optimized <- combs$Optimized_ts$Optimized * bias
    combs$Optimized_ts$Residuals <- combs$Optimized_ts$Optimized -
                                        combs$Optimized_ts$Observations
    
    trains <- combs$Optimized_ts$Train_or_test == "Train"
    tests <- combs$Optimized_ts$Train_or_test == "Test"
    
    if(sum(tests, na.rm=TRUE) == 0) {
        traingof <- hydroGOF::gof(combs$Optimized_ts$Optimized[trains], 
                                  combs$Optimized_ts$Observations[trains])  
        gofs <- data.frame(Train = traingof)
    } else {
        traingof <- hydroGOF::gof(combs$Optimized_ts$Optimized[trains], 
                                  combs$Optimized_ts$Observations[trains]) 
        
        testgof <- hydroGOF::gof(combs$Optimized_ts$Optimized[tests], 
                                 combs$Optimized_ts$Observations[tests])
        
        bothgof <- hydroGOF::gof(combs$Optimized_ts$Optimized, 
                                 combs$Optimized_ts$Observations)
        
        gofs <- data.frame(Train = traingof,
                           Test = testgof,
                           Together = bothgof)
    }
    
    combs$Goodness_of_fit <- gofs
    
    return(combs)
}


train_test <- function(flow, train, sampling = "series", warned = FALSE) {
    vals <- which(complete.cases(flow))
    if(sampling == "series") {
        
        
        train_ <- 1:(round(length(vals)*train, 0))
        train_ <- vals[train_]
        test_ <- vals[!vals %in% train_]
        
        if(length(test_) == 0 && !warned) {
            warning("Train period covers the entire timeseries.")
            test_ <- train_
            warned <- TRUE
        }
    }
    
    if(sampling == "random") {
        ntrain <- round(length(vals)*train, 0)
        train_ <- sample(vals,ntrain)
        train_ <- sort(train_)
        test_ <- vals[!vals %in% train_]
        
        if(length(test_) == 0 && !warned) {
            warning("Train period covers the entire timeseries.")
            test_ <- train_
        }
    }
    
    return(list(train = train_, test = test_, warned = warned))
}

combine_timeseries <- function(flow, 
                               optim_method, 
                               sampling, 
                               train,
                               bias_correction,
                               warned_overfit,
                               warned_train) {
    
    tt <- train_test(flow, train, sampling, warned_train)
    warned_train <- tt$warned
    warned_overfit <- warned_overfit
    
    # warn about overfitting?
    test <- length(tt$train) < ncol(flow)-2
    if(test && optim_method == "OLS" && !warned_overfit) {
        warning("Forecast combination is underdetermined, which leads to 
                extreme overfitting")
        warned_overfit <- TRUE
    }
    
    ncols <- ncol(flow)
    
    if(length(tt$test) == 0) {
        fcast <- suppressMessages(
            ForecastComb::foreccomb(as.matrix(flow[tt$train, ncols]),
                                    as.matrix(flow[tt$train, -c(1, ncols)])))
    } else {
        fcast <- suppressMessages(
            ForecastComb::foreccomb(as.matrix(flow[tt$train, ncols]),
                                    as.matrix(flow[tt$train, -c(1, ncols)]),
                                    as.matrix(flow[tt$test, ncols]), 
                                    as.matrix(flow[tt$test, -c(1, ncols)])))
    }
    
    
    if(optim_method == "factorCLS") {
        # This is the function ForecastComb::comb_CLS() in a factorized form 
        # because the original unfactorized ForecastComb function results often 
        # in no solutions error in solve.QP(). The fix is sourced from 
        # StackOverflow: https://stackoverflow.com/a/28388394
        combs <- forecastcomb_comb_CLS(fcast) 
    } else if (optim_method == "CLS") {
        combs <- ForecastComb::comb_CLS(fcast)
    } else if (optim_method == "OLS") {
        combs <- ForecastComb::comb_OLS(fcast)
    } else if (optim_method == "BG") {
        combs <- ForecastComb::comb_BG(fcast)
    } else if (optim_method == "EIG1") {
        combs <- ForecastComb::comb_EIG1(fcast)
    } else if (optim_method == "EIG2") {
        combs <- ForecastComb::comb_EIG2(fcast)
    } else if (optim_method == "EIG3") {
        combs <- ForecastComb::comb_EIG3(fcast)
    } else if (optim_method == "EIG4") {
        combs <- ForecastComb::comb_EIG4(fcast)
    } else if (optim_method == "InvW") {
        combs <- ForecastComb::comb_InvW(fcast)
    } else if (optim_method == "LAD") {
        combs <- ForecastComb::comb_LAD(fcast)
    } else if (optim_method == "MED") {
        combs <- ForecastComb::comb_MED(fcast)
    } else if (optim_method == "NG") {
        combs <- ForecastComb::comb_NG(fcast)
    } else if (optim_method == "SA") {
        combs <- ForecastComb::comb_SA(fcast)
    } else if (optim_method == "TA") {
        combs <- ForecastComb::comb_TA(fcast)
    } else if (optim_method == "WA") {
        combs <- ForecastComb::comb_WA(fcast)
    } else if (optim_method == "auto") {
        combs <- ForecastComb::auto_combine(fcast)
    } 
    
    # gather weights
    method <- combs$Method
    models <- combs$Models
    weights <- combs$Weights
    names(weights) <- models
    weights[is.na(weights)] <- 0
    
   
    # get intercept
    if(is.null(combs$Intercept)) {
        int <- 0
    } else {
        int <- combs$Intercept
    }
    
    # Create the optimized timeseries and make a tsibble
    w <- c(int, weights)
    modelind <- which(colnames(flow) %in% names(weights), arr.ind=TRUE)
    p <- t(cbind(1,as.matrix(flow[,modelind])))
    Opt <- as.vector(w %*% p)
    
    whichtt <- rep(NA, length(Opt))
    whichtt[tt$test] <- "Test"
    whichtt[tt$train] <- "Train"
    
    pred <- suppressMessages(data.frame(Date = flow$Date, 
                                        Observations = flow$observations,
                                        Optimized = Opt,
                                        Residuals = Opt - flow$observations,
                                        Train_or_test = whichtt) %>%
                                 tsibble::as_tsibble())
    
    
    # Goodness of fit
    trains <- whichtt == "Train" 
    tests <- whichtt == "Test"
    if(sum(tests, na.rm=TRUE) == 0) {
        traingof <- hydroGOF::gof(as.numeric(Opt[tt$train]),
                                  as.numeric(flow$observations[tt$train]),
                                  na.rm = TRUE)  
        gofs <- data.frame(Train = traingof)
    } else {
        traingof <- hydroGOF::gof(as.numeric(Opt[tt$train]),
                                  as.numeric(flow$observations[tt$train]),
                                  na.rm = TRUE)
        testgof <- hydroGOF::gof(as.numeric(Opt[tt$test]),
                                 as.numeric(flow$observations[tt$test]),
                                 na.rm = TRUE)
        bothgof <- hydroGOF::gof(as.numeric(Opt),
                                 as.numeric(flow$observations),
                                 na.rm = TRUE)
        
        gofs <- data.frame(Train = traingof,
                           Test = testgof,
                           Together = bothgof)
    }
    
    
    output <- list(Method = method,
                   Weights = weights,
                   Intercept = int,
                   Optimized_ts = pred,
                   Goodness_of_fit = gofs,
                   warned_overfit = warned_overfit,
                   warned_train = warned_train)
    
    
    if (bias_correction) {
        output <- bias_correct(output)
    } 
    
    return(output)
}



combine_monthly <- function(flow, 
                            optim_method, 
                            sampling, 
                            train, 
                            bias_correction,
                            warned_overfit,
                            warned_train) {
    
    # prepare
    Date <- NULL
    tempcombs <- list()
    ncols <- ncol(flow)
    
    
    
    ###############################
    # Do combination for each month
    for (m in 1:12) {
        month <- lubridate::month(flow$Date) == m
        mdata <- flow[month,]
        
        tempcombs[[m]] <- combine_timeseries(mdata,
                                             optim_method, 
                                             sampling,
                                             train,
                                             bias_correction,
                                             warned_overfit,
                                             warned_train)
        
        warned_overfit <- tempcombs[[m]]$warned_overfit
        warned_train <- tempcombs[[m]]$warned_train
        
    }
    
    method <- tempcombs[[m]]$Method
    
    weights <- lapply(tempcombs, function(x) x$Weights)
    
    weights <- cbind(Month = c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG",
                               "SEP","OCT","NOV","DEC"),
                     do.call("bind_rows", weights))
    
    
    if(is.null(tempcombs[[1]]$Intercept)) {
        int <- 0
    } else {
        int <- sapply(tempcombs, function(x) x$Intercept)
        names(int) <- c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG",
                        "SEP","OCT","NOV","DEC")
    }
    
    if(bias_correction) {
        bias <- sapply(tempcombs, function(x) x$Bias_correction)
        names(bias) <- c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG",
                        "SEP","OCT","NOV","DEC")
    } else {
        bias <- FALSE
    }
    
    # combine and sort the optimized timeseries
    opt_ts <- lapply(tempcombs, function(x) x$Optimized_ts)
    opt_ts <- do.call("bind_rows", opt_ts) %>%
        tibble::as_tibble() %>%
        dplyr::arrange(Date) %>%
        tsibble::as_tsibble(index = Date)
    
    
    # goodness-of-fit
    trains <- opt_ts$Train_or_test == "Train" 
    tests <- opt_ts$Train_or_test == "Test"
    if(sum(tests, na.rm=TRUE) == 0) {
        traingof <- hydroGOF::gof(opt_ts$Optimized[trains], 
                                  opt_ts$Observations[trains])  
        gofs <- data.frame(Train = traingof)
    } else {
        traingof <- hydroGOF::gof(opt_ts$Optimized[trains], 
                                  opt_ts$Observations[trains]) 
        
        testgof <- hydroGOF::gof(opt_ts$Optimized[tests], 
                                 opt_ts$Observations[tests])
        
        bothgof <- hydroGOF::gof(opt_ts$Optimized, 
                                 opt_ts$Observations)
        
        gofs <- data.frame(Train = traingof,
                           Test = testgof,
                           Together = bothgof)
    }

    gofs <- c(list(gofs), 
              lapply(tempcombs, function(x) x$Goodness_of_fit))
    names(gofs) <- c("Entire timeseries","JAN","FEB","MAR","APR","MAY","JUN",
                     "JULY","AUG", "SEP","OCT","NOV","DEC")
    
    
    
    output <- list(Method = method,
                   Weights = weights,
                   Intercept = int,
                   Optimized_ts = opt_ts,
                   Bias_correction = bias,
                   Goodness_of_fit = gofs,
                   warned_overfit = warned_overfit,
                   warned_train = warned_train)
    
    return(output)
}
