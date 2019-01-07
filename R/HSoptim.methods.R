######
# methods for HSoptim
#####


#' @export
plot.HSoptim <- function(HSoptim, 
                         plot_type = "timeseries", 
                         gof_stat = c("NSE", "RMSE", "ME", "R2", "KGE", "PBIAS.."), 
                         interactive=FALSE) {
    
    stat_names <- names(HSoptim)
    forecasts <- list()
    observations <- list()
    weights <- vector()
    gofs <- list()
    for(i in seq_along(HSoptim)) {
        forecasts[[ stat_names[i] ]] <- tibble::add_column(HSoptim[[i]]$Forecast_train_test, 
                                                           Type = "Forecast", 
                                                           Station = stat_names[i],
                                                           .before=1)
        observations[[ stat_names[i] ]] <- tibble::add_column(HSoptim[[i]]$Observations, 
                                                              Type = "Observations", 
                                                              Station = stat_names[i],
                                                              .before=1)
        weights <- bind_rows(weights, data.frame(Station =stat_names[i], 
                                                 Forecast = names(HSoptim[[i]]$Forecast_weights),
                                                 Weight = HSoptim[[i]]$Forecast_weights,
                                                 stringsAsFactors = FALSE))
        ## hack because tibble:add_column commented below throws a weird error 
        # that an atomic character vector would not be atomic.
        temp <- tibble::as.tibble(data.frame(t(HSoptim[[i]]$Goodness_of_fit)))
        temp$Period <- colnames(HSoptim[[i]]$Goodness_of_fit)
        temp$Station <- rep(stat_names[i], nrow(temp))
        gofs [[ stat_names[i] ]] <- temp
    }
    weights <- weights[-1,]
    
    if (plot_type == "timeseries") {
        forecasts <- do.call("bind_rows", forecasts)
        observations <- do.call("bind_rows", observations)
        p <- ggplot() +
            geom_line(data = forecasts, aes(x=Date, 
                                            y=Forecast, 
                                            color='Forecast'), 
                      size=1, 
                      color='blue') +
            geom_line(data = observations, aes(x=Date, 
                                               y=Observations, 
                                               color='Observations'), 
                      size=1, 
                      color='red') +
            facet_wrap(~Station, scales = "free_x") + 
            theme_bw() + 
            ylab('Discharge Q (m3/s)') +
            xlab('') +
            ggtitle(paste0('Forecasted vs. observed streamflow using ',
                           HSoptim[[1]]$Method)) + 
            theme(axis.text.x = element_text(angle = 45, 
                                             hjust = 1))

        
        
    } else if (plot_type == "residuals") {
        forecasts <- do.call("bind_rows", forecasts)
        observations <- do.call("bind_rows", observations)
        p <- ggplot() +
            geom_line(data = forecasts, 
                      aes(x=Date, y=Residuals), 
                      size=1, 
                      color='blue') +
            facet_wrap(~Station, scales = "free") + 
            theme_bw() + 
            ylab('Residual discharge Q (m3/s)') +
            xlab('') +
            ggtitle(paste0('Forecasted-observed streamflow residuals using ',
                           HSoptim[[1]]$Method)) + 
            theme(axis.text.x = element_text(angle = 45, 
                                             hjust = 1))

        
        
    } else if (plot_type == "weights") {
        p <- ggplot(weights) + 
            geom_bar(aes(x=Station, y=Weight), stat="identity") +
            facet_wrap(~Forecast) +
            theme_bw() + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle(paste0('Weights of forecasts for each observation station using ',
                           HSoptim[[1]]$Method))

        
        
    } else if (plot_type == "gof") {
        gofs <- do.call("bind_rows", gofs)
        
        suppressWarnings(if(gof_stat == "all") {
            gof_stat <- colnames(gofs)
        })
        keep <- c("Period", "Station", gof_stat)
        keep <- colnames(gofs) %in% keep
        
        gofs <- tidyr::gather(gofs[,keep], Metric, Value, -Period, -Station)
        
        p <- ggplot(gofs) +
            geom_bar(aes(x=Station, y=Value, fill=Period ), 
                     stat="identity", 
                     position = "dodge") +
            facet_wrap(~Metric, scales="free_y") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle(paste0('Goodness of fit metrics for each observation station using ',
                           HSoptim[[1]]$Method)) +
            scale_fill_manual(values = c('#edf8b1','#7fcdbb','#2c7fb8'))
        
    }
    
    if(interactive) {
        if(!require(plotly)) stop("Package 'plotly' required for interactive plots. 
                                  Run install.packages('plotly').")
        print(plotly::ggplotly(p))
    } else {
        print(p)
    }
    
    return(p)
} 
