bias_correct <- function(combs, type = "ratio") {
    
    # get bias
    if (type == "ratio") {
        bias <- (100+(-combs$Goodness_of_fit[6, 1])) / 100
        combs[["Bias_correction"]] <- -combs$Goodness_of_fit[6, 1]
        
        # update timeseries with bias
        combs$Optimized_ts$Optimized <- combs$Optimized_ts$Optimized * bias
    }
    
    if (type == "abs") {
        bias <- combs$Goodness_of_fit[1,1]
        combs[["Bias_correction"]] <- -combs$Goodness_of_fit[1, 1]
        
        # update timeseries with bias
        combs$Optimized_ts$Optimized <- combs$Optimized_ts$Optimized + bias
        
    }
    
    #update residuals
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


train_test <- function(flow, train, sampling = "random", warned = FALSE) {
    vals <- which(complete.cases(flow))
    
    if(is.logical(train)) {
        train_ <- which(train)
        test_ <- which(!train)
        
    } else if(sampling == "series") {
        train_ <- 1:(round(length(vals)*train, 0))
        train_ <- vals[train_]
        test_ <- vals[!vals %in% train_]
        
    } else if(sampling == "random") {
        ntrain <- round(length(vals)*train, 0)
        train_ <- sample(vals,ntrain)
        train_ <- sort(train_)
        test_ <- vals[!vals %in% train_]
        
    }
    
    if(length(test_) == 0 && !warned) {
        warning("Train period covers the entire timeseries.")
        test_ <- train_
        warned <- TRUE
    }
    
    return(list(train = train_, test = test_, warned = warned))
}

combine_timeseries <- function(flow, 
                               optim_method, 
                               sampling, 
                               train,
                               bias_correction,
                               log,
                               warned_overfit,
                               warned_train) { 
    
    
    
    tt <- train_test(flow, train, sampling, warned_train)
    warned_train <- tt$warned
    warned_overfit <- warned_overfit
    
    # warn about overfitting?
    test <- length(tt$train) < ncol(flow)-2
    if(test && optim_method %in% c("OLS", "GRC") && !warned_overfit) {
        warning("Forecast combination is underdetermined, which leads to 
                extreme overfitting")
        warned_overfit <- TRUE
    }
    
    # log transform?
    if(log) flow[,-1] <- log(flow[,-1])
    
    #ncols <- ncol(flow)
    
    ###########
    # Combine
    combs <- combinations(flow[tt$train,], optim_method)
    ###########
    
    # gather weights
    method <- combs$Method
    models <- combs$Models
    weights <- combs$Weights
    #names(weights) <- models
    weights[is.na(weights)] <- 0
    
    # get intercept
    int <- combs$Intercept

    # Create the optimized timeseries and make a tsibble
    w <- c(int, weights)
    modelind <- which(colnames(flow) %in% models, arr.ind=TRUE)
    p <- t(cbind(1,as.matrix(flow[,modelind])))
    Opt <- as.vector(w %*% p)
    
    if(log) {
        Opt <- exp(Opt)
        flow[,-1] <- exp(flow[,-1])
    }
    
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
                   Bias_correction = 0,
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
                            log,
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
                                             log,
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
    
    int <- sapply(tempcombs, function(x) x$Intercept)
    names(int) <- c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG",
                    "SEP","OCT","NOV","DEC")
    
    

    bias <- sapply(tempcombs, function(x) x$Bias_correction)
    names(bias) <- c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG",
                     "SEP","OCT","NOV","DEC")

    
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


combine_annual <- function(flow, 
                           optim_method, 
                           sampling, 
                           train, 
                           bias_correction,
                           log,
                           warned_overfit,
                           warned_train) {

    # determine which years to combine in the first station iteration
    years <- flow$Date %>% lubridate::year() %>% unique
    ntrain <- round( length(years)*train, 0)
    if(ntrain == 0) ntrain <- 1
    
    if(sampling == "random") {
        train_years <- sample(years,ntrain)
        train <- lubridate::year(flow$Date) %in% train_years
    } else if (sampling == "series") {
        train_years <- years[1:ntrain]
        train <- lubridate::year(flow$Date) %in% train_years
    }
    

    output <- combine_timeseries(tibble::as_tibble(flow), 
                                 optim_method, 
                                 sampling,
                                 train,
                                 bias_correction,
                                 log,
                                 warned_overfit,
                                 warned_train)
    
    return(output)
    
}

# # This is function ForecastComb::comb_CLS(), but edited according to 
# # https://stackoverflow.com/a/28388394. 
# # Edits marked with ###.
# forecastcomb_comb_CLS <- function (x) {
#     
#     if (class(x) != "foreccomb") 
#         stop("Data must be class 'foreccomb'. See ?foreccomb to bring data",
#              " in in a correct format.", call. = FALSE)
#     observed_vector <- x$Actual_Train
#     prediction_matrix <- x$Forecasts_Train
#     modelnames <- x$modelnames
#     p <- NCOL(prediction_matrix)
#     Rinv <- solve(safe_chol(t(prediction_matrix) %*% prediction_matrix))
#     C <- cbind(rep(1, p), diag(p))
#     b <- c(1, rep(0, p))
#     d <- t(observed_vector) %*% prediction_matrix
#     nn2 <- sqrt(norm(d,"2")) ###
#     qp1 <- solve.QP(Dmat = Rinv*nn2, factorized = TRUE, dvec = d/(nn2^2),  ###
#                    Amat = C, bvec = b, meq = 1)
#     weights <- unname(qp1$sol)
#     fitted <- as.vector(weights %*% t(prediction_matrix))
#     accuracy_insample <- forecast::accuracy(fitted, observed_vector)
#     if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
#         result <- structure(list(
#                      Method = "Constrained Least Squares Regression", 
#                      Models = modelnames, 
#                      Weights = weights, 
#                      Fitted = fitted, 
#                      Accuracy_Train = accuracy_insample, 
#                      Input_Data = list(Actual_Train = x$Actual_Train,
#                                        Forecasts_Train = x$Forecasts_Train)), 
#                 class = c("foreccomb_res"))
#         rownames(result$Accuracy_Train) <- "Training Set"
#     }
#     if (is.null(x$Forecasts_Test) == FALSE) {
#         newpred_matrix <- x$Forecasts_Test
#         pred <- as.vector(weights %*% t(newpred_matrix))
#         if (is.null(x$Actual_Test) == TRUE) {
#             result <- structure(list(
#                      Method = "Constrained Least Squares Regression", 
#                      Models = modelnames, 
#                      Weights = weights, 
#                      Fitted = fitted, 
#                      Accuracy_Train = accuracy_insample, 
#                      Forecasts_Test = pred, 
#                      Input_Data = list(Actual_Train = x$Actual_Train, 
#                                        Forecasts_Train = x$Forecasts_Train, 
#                                        Forecasts_Test = x$Forecasts_Test)), 
#                 class = c("foreccomb_res"))
#             rownames(result$Accuracy_Train) <- "Training Set"
#         }
#         else {
#             newobs_vector <- x$Actual_Test
#             accuracy_outsample <- forecast::accuracy(pred, newobs_vector)
#             result <- structure(list(
#                      Method = "Constrained Least Squares Regression", 
#                      Models = modelnames, 
#                      Weights = weights, 
#                      Fitted = fitted, 
#                      Accuracy_Train = accuracy_insample, 
#                      Forecasts_Test = pred, 
#                      Accuracy_Test = accuracy_outsample, 
#                      Input_Data = list(Actual_Train = x$Actual_Train,
#                                        Forecasts_Train = x$Forecasts_Train, 
#                                        Actual_Test = x$Actual_Test, 
#                                        Forecasts_Test = x$Forecasts_Test)),
#                 class = c("foreccomb_res"))
#             rownames(result$Accuracy_Train) <- "Training Set"
#             rownames(result$Accuracy_Test) <- "Test Set"
#         }
#     }
#     return(result)
# }


# from package 'lme4'; needed in forecastcomb_comb_CLS()
safe_chol <- function(m) {
    if (all(m==0)) return(m)
    if (nrow(m)==1) return(sqrt(m))
    if (all(dmult(m,0)==0)) {  ## diagonal
        return(diag(sqrt(diag(m))))
    }
    ## attempt regular Chol. decomp
    if (!inherits(try(cc <- chol(m),silent=TRUE),"try-error"))
        return(cc)
    ## ... pivot if necessary ...
    cc <- suppressWarnings(chol(m,pivot=TRUE))
    oo <- order(attr(cc,"pivot"))
    cc[,oo]
    ## FIXME: pivot is here to deal with semidefinite cases,
    ## but results might be returned in a strange format: TEST
}

# from package 'lme4'; needed in safe_chol()
dmult <- function(m,s) {
    diag(m) <- diag(m)*s
    m
}

# Implements Granger-Ramanathan type B 
# (Arsenault et al, 2016, 10.1016/j.jhydrol.2015.09.001)
# Function is a ForecastComb::comb_OLS with small modification. 
# comb_GR <- function (x, type) {
#     if (class(x) != "foreccomb") 
#         stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data",
#              " in correct format.", call. = FALSE)
#     observed_vector <- x$Actual_Train
#     prediction_matrix <- x$Forecasts_Train
#     modelnames <- x$modelnames
#     lin_model <- lm(observed_vector ~ 0 + prediction_matrix)
#     if (type == "A") weights <- lin_model$coef
#     if (type == "B") weights <- unname(lin_model$coef/sum(lin_model$coef))
#     fitted <- as.vector(weights %*% t(prediction_matrix))
#     intercept <- NA
#     accuracy_insample <- forecast::accuracy(fitted, observed_vector)
#     if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
#         result <- ForecastComb::foreccomb_res(method = 
#                                     paste0("Granger-Ramanathan type ",type),
#                         modelnames = modelnames, 
#                         weights = weights, 
#                         #intercept = intercept, 
#                         fitted = fitted, 
#                         accuracy_insample = accuracy_insample, 
#                         input_data = list(Actual_Train = x$Actual_Train, 
#                                           Forecasts_Train = x$Forecasts_Train))#, 
#         #predict = predict.comb_OLS)
#     }
#     if (is.null(x$Forecasts_Test) == FALSE) {
#         newpred_matrix <- x$Forecasts_Test
#         pred <- as.vector(lin_model$coef %*% t(newpred_matrix))
#         if (is.null(x$Actual_Test) == TRUE) {
#             result <- ForecastComb::foreccomb_res(method = 
#                                         paste0("Granger-Ramanathan type ",
#                                                type), 
#                         modelnames = modelnames, 
#                         weights = weights, 
#                         #intercept = intercept, 
#                         fitted = fitted, 
#                         accuracy_insample = accuracy_insample, 
#                         pred = pred, 
#                         input_data = list(Actual_Train = x$Actual_Train, 
#                                           Forecasts_Train = x$Forecasts_Train, 
#                                           Forecasts_Test = x$Forecasts_Test))#, 
#             #predict = predict.comb_OLS)
#         }
#         else {
#             newobs_vector <- x$Actual_Test
#             accuracy_outsample <- forecast::accuracy(pred, newobs_vector)
#             result <- ForecastComb::foreccomb_res(method = 
#                                         paste0("Granger-Ramanathan type ",
#                                                type), 
#                         modelnames = modelnames, 
#                         weights = weights, 
#                         #intercept = intercept, 
#                         fitted = fitted, 
#                         accuracy_insample = accuracy_insample, 
#                         pred = pred, 
#                         accuracy_outsample = accuracy_outsample, 
#                         input_data = list(Actual_Train = x$Actual_Train, 
#                                           Forecasts_Train = x$Forecasts_Train, 
#                                           Actual_Test = x$Actual_Test, 
#                                           Forecasts_Test = x$Forecasts_Test))#, 
#                         #predict = predict.comb_OLS)
#         }
#     }
#     return(result)
# }


#### Draws heavily from ForecastComb package
combinations <- function(flowmat, type="CLS") {
    
    # remove collinear timeseries
    flowmat <- remove_collinear(flowmat)
    
    ncols <- ncol(flowmat)
    obs <- flowmat$observations %>% unname
    predmat <- flowmat[,-c(1, ncols)] %>% as.matrix
    models <- colnames(predmat)
    intercept <- 0
    
    if(type == "CLS") {
        # This is borrows from ForecastComb::comb_CLS(), but edited  
        # according to https://stackoverflow.com/a/28388394. 
        method <- c("Constrained Least Squares; 0 < weights < 1; ",
                    "sum(weights) = 1; intercept = 0")
        p <- NCOL(predmat)
        Rinv <- solve(safe_chol(t(predmat) %*% predmat))
        C <- cbind(rep(1, p), diag(p))
        b <- c(1, rep(0, p))
        d <- t(obs) %*% predmat
        nn2 <- sqrt(norm(d,"2"))
        qp1 <- quadprog::solve.QP(Dmat = Rinv*nn2, 
                        factorized = TRUE, 
                        dvec = d/(nn2^2), 
                        Amat = C, 
                        bvec = b, 
                        meq = 1)
        weights <- unname(qp1$sol)
        
    } else if (type == "GRA") {
        method <- "Granger-Ramanathan Type A - Least Squares; intercept = 0"
        lin_model <- lm(obs ~ 0 + predmat)
        weights <- lin_model$coef
        
    } else if (type == "GRB") {
        method <- paste0("Granger-Ramanathan Type A - Least Squares; ",
                         "sum(weights) = 1; intercept = 0")
        lin_model <- lm(obs ~ 0 + predmat)
        weights <- unname(lin_model$coef/sum(lin_model$coef))
        
    } else if (type %in% c("GRC", "OLS") ) {
        method <- paste0("Granger-Ramanathan Type A - Least Squares; ",
                  "Ordinary Least Squares")
        lin_model <- lm(obs ~ predmat)
        weights <- lin_model$coef[-1]
        intercept <- lin_model$coef[1]
        
    } else if (type == "NNLS") {
        method <- paste0("Non-Negative Least Squares; weights > 0;",
                         "intercept = 0")
        p <- NCOL(predmat)
        Rinv <- solve(safe_chol(t(predmat) %*% predmat))
        #C <- cbind(rep(1, p), diag(p))
        C <- cbind(rep(0, p), diag(p))
        #b <- c(1, rep(0, p))
        b <- c(0, rep(0, p))
        d <- t(obs) %*% predmat
        nn2 <- sqrt(norm(d,"2")) ###
        qp1 <- quadprog::solve.QP(Dmat = Rinv*nn2, 
                                  factorized = TRUE, 
                                  dvec = d/(nn2^2), 
                                  Amat = C, 
                                  bvec = b)
        weights <- unname(qp1$sol)

    }  else if (type == "BG") {
        method <- "Bates-Granger; sum(weights) = 1; intercept = 0"
        errmat <- obs - predmat
        sample_msqu_pred_error <- (t(errmat) %*% errmat)/length(obs)
        weights <- diag(sample_msqu_pred_error)^(-1)/
                        sum(diag(sample_msqu_pred_error)^(-1))

    } else if (type == "InvW") {
        method <- "Inverse Rank combination; sum(weights) = 1; intercept = 0"
        errmat <- obs - predmat
        sse <- colSums((errmat)^2)
        ranks <- rank(sse)
        inv_ranks <- 1/ranks
        weights <- inv_ranks/sum(inv_ranks)

    } else if (type == "EIG1") {
        method <- paste0("Standard Eigenvector combination;",
                         " sum(weights) = 1; intercept = 0")
        errmat <- obs - predmat
        sample_msqu_pred_error <- (t(errmat) %*% errmat)/length(obs)
        eigen_decomp <- eigen(sample_msqu_pred_error)
        ds <- colSums(eigen_decomp$vectors)
        adj_eigen_vals <- eigen_decomp$values/(ds^2)
        min_idx <- which.min(adj_eigen_vals)
        weights <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
        
    } else if (type == "EIG2") {
        method <- paste0("Bias-corrected Eigenvector combination;",
                         " sum(weights) = 1")
        mean_obs <- mean(obs)
        mean_preds <- colMeans(predmat)
        centered_obs <- obs - mean_obs
        centered_preds <- scale(predmat, scale = FALSE)
        omega_matrix <- t(centered_obs - centered_preds) %*% 
                            (centered_obs - centered_preds)/length(obs)
        eigen_decomp <- eigen(omega_matrix)
        ds <- colSums(eigen_decomp$vectors)
        adj_eigen_vals <- eigen_decomp$values/(ds^2)
        min_idx <- which.min(adj_eigen_vals)
        weights <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
        intercept <- as.numeric(mean_obs - t(mean_preds) %*% weights)
        
    } else if (type == "best") {
        method <- "Best individual ensemble member"
        
        rmse <- rep(NA, ncol(predmat))
        for(i in 1:ncol(predmat)) {
            rmse[i] <- hydroGOF::rmse(predmat[,i], obs)
        }
        min <- which(rmse == min(rmse))
        weights <- rep(0, ncol(predmat))
        weights[min] <- 1
        names(weights) <- colnames(predmat)

    } else {
        stop("Unknown optimization method: ", type)
    }

    names(weights) <- models
    output <- list(Method = method,
                   Models = models,
                   Weights = weights,
                   Intercept = intercept)
    
    return(output)
}

# remove collinear timeseries. modified from ForecastComb::remove_collinear
remove_collinear <- function (flow) {
    ncols <- ncol(flow)
    obs <- flow$observations
    predmat <- flow[,-c(1,ncols)] %>% as.matrix
    
    # remove collinear timeseries according to RMSE, until prediction matrix
    # is full rank.
    repeat {
        repeat_this <- Matrix::rankMatrix(predmat)[1] == ncol(predmat)
        if (repeat_this == TRUE) {
            break
        }
        ranks <- rep(NA, ncol(predmat))
        for (i in 1:ncol(predmat)) {
            ranks[i] <- Matrix::rankMatrix(predmat[, -i])[1]
        }
        maxrank <- which(ranks == max(ranks))
        remove <- rep(0, ncol(predmat))
        for (i in maxrank) {
            remove[i] <- sqrt(mean((predmat[,i] - obs)^2, na.rm = TRUE))
        }
        remove <- which(remove == max(remove))[1]
        predmat <- predmat[, -remove]
    }

    keep <- colnames(flow) %in% c("Date", "observations", colnames(predmat))
    output <- flow[,which(keep)]
    return(output)
}