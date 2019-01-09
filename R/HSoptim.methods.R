######
# methods for HSoptim
#####


#' @export
plot.HSoptim <- function(x, ...) {
    plot_HSoptim(x, ...)
}

#' Plot HSoptim
#' 
#' Plots \code{HSoptim} object, obtained with \code{\link{optimise_point}}.
#' 
#' Four types of plots are available:
#' \itemize{
#'   \item "timeseries": plots a timeseries for each optimized station
#'   \item "residuals": plots residuals at each optimized station
#'   \item "weights": plots weights of runoff timeseries used to get the 
#'     optimized timeseries
#'   \item "gof": plots goodness-of-fit statistics for each station
#' }
#' 
#' @param HSoptim An \code{HSoptim} object
#' @param plot_type One of "timeseries", "residuals", "weights", or "gof". 
#'   Defaults to "timeseries"
#' @param gof_stat Statistics to draw plot from. For options, see 
#'   \code{\link[hydroGOF]{gof}}. Only used if \code{plot_type == "gof"}.
#' @param interactive Whether or not use package \code{plotly} to draw an
#'   interactive plot, rather than static ggplot.
#' 
#' 
#' @export
plot_HSoptim <- function(HSoptim,
                         plot_type = c("timeseries", 
                                       "residuals", 
                                       "weights", 
                                       "gof"),
                         gof_stat = c("NSE", 
                                      "RMSE", 
                                      "ME", 
                                      "R2", 
                                      "KGE", 
                                      "PBIAS.."),
                         interactive=FALSE) {
    
    Date <- Forecast <- Observations <- Residuals <- Station <- Weight <- NULL
    Metric <- Value <- Period <- plotly <- HSgrid <- NULL
    
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
        p <- ggplot2::ggplot() +
            ggplot2::geom_line(data = forecasts, 
                               ggplot2::aes(x=Date, 
                                            y=Forecast, 
                                            color='Forecast'),
                               size=1, 
                               color='blue') +
            ggplot2::geom_line(data = observations, 
                               ggplot2::aes(x=Date, 
                                            y=Observations, 
                                            color='Observations'),
                               size=1, 
                               color='red') +
            ggplot2::facet_wrap(~Station, scales = "free_x") + 
            ggplot2::theme_bw() + 
            ggplot2::ylab('Discharge Q (m3/s)') +
            ggplot2::xlab('') +
            ggplot2::ggtitle(paste0('Forecasted vs. observed streamflow using ',
                           HSoptim[[1]]$Method)) + 
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                             hjust = 1))

        
        
    } else if (plot_type == "residuals") {
        forecasts <- do.call("bind_rows", forecasts)
        observations <- do.call("bind_rows", observations)
        p <- ggplot2::ggplot() +
            ggplot2::geom_line(data = forecasts, 
                               ggplot2::aes(x=Date, y=Residuals), 
                               size=1, 
                               color='blue') +
            ggplot2::facet_wrap(~Station, scales = "free") + 
            ggplot2::theme_bw() + 
            ggplot2::ylab('Residual discharge Q (m3/s)') +
            ggplot2::xlab('') +
            ggplot2::ggtitle(paste0('Forecasted-observed streamflow residuals using ',
                           HSoptim[[1]]$Method)) + 
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                             hjust = 1))

        
        
    } else if (plot_type == "weights") {
        p <- ggplot2::ggplot(weights) + 
            ggplot2::geom_bar(ggplot2::aes(x=Station, y=Weight), 
                              stat="identity") +
            ggplot2::facet_wrap(~Forecast) +
            ggplot2::theme_bw() + 
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
            ggplot2:: ggtitle(paste0('Weights of forecasts for each observation 
                                     station using ', HSoptim[[1]]$Method))

        
        
    } else if (plot_type == "gof") {
        gofs <- do.call("bind_rows", gofs)
        
        suppressWarnings(if(gof_stat == "all") {
            gof_stat <- colnames(gofs)
        })
        keep <- c("Period", "Station", gof_stat)
        keep <- colnames(gofs) %in% keep
        
        gofs <- tidyr::gather(gofs[,keep], Metric, Value, -Period, -Station)
        
        p <- ggplot2::ggplot(gofs) +
            ggplot2::geom_bar(ggplot2::aes(x=Station, 
                                           y=Value, 
                                           fill=Period ), 
                              stat="identity",
                              position = "dodge") +
            ggplot2::facet_wrap(~Metric, scales="free_y") +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
            ggplot2::ggtitle(paste0('Goodness of fit metrics for each observation station using ',
                           HSoptim[[1]]$Method)) +
            ggplot2::scale_fill_manual(values = c('#edf8b1','#7fcdbb','#2c7fb8'))
        
    }
    
    if(interactive) {
        if(!requireNamespace(plotly)) stop("Package 'plotly' required for interactive plots. 
                                  Run install.packages('plotly').")
        print(plotly::ggplotly(p))
    } else {
        print(p)
    }
    
    return(p)
} 
