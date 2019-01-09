#######
# methods for HSflow
#######

#' @export
plot.HSflow <- function(x, ...) {
    if(!hasArg("HSobs")) {
        stop("Plotting HSflow requires HSobs.")
        HSobs <- NULL
    }
    plot_HSflow(x,HSobs)
}

#' Plot HSflow
#' 
#' Plots a timeseries of all predictions in HSflow.
#' 
#' @param HSflow An \code{HSflow} object
#' @param HSobs An \code{HSobs} object
#' 
#' @export
plot_HSflow <- function(HSflow, HSobs) { 
    if (!requireNamespace("ggplot2") ) stop('Package "ggplot2" is required for 
                                          the plot function. Please install 
                                          ggplot2.')
    
    Type <- NULL
    Date <- NULL
    Station <- NULL
    Q <- NULL
    
    stations <- HSobs$riverIDs
    ind <- vector()
    for (i in 1:length(stations)) {
        ind[i] <- which(colnames(HSflow$discharge[[1]]) == stations[i])
    }
    
    
    data <- tibble::add_column(HSobs$Observations, Type ="Observations", .before=1)
    
    ndis <- length(HSflow$discharge)
    names <- names(HSflow$discharge)
    stat_names <- colnames(HSobs$Observations)[2:ncol(HSobs$Observations)]
    
    if(is.null(names)) {
        if (ndis == 1) {
            names <- "Q"
        } else {
            names <- paste0("Q", 1:ndis)
        }
    }
    
    for (i in 1:ndis) {
        temp <- HSflow$discharge[[i]][,c(1,ind)] %>% 
            tibble::as.tibble() %>%
            tibble::add_column(Type = names[i]) %>%
            dplyr::select(Type, Date, dplyr::everything())
        colnames(temp) <- colnames(data)
        data <- dplyr::bind_rows(data, temp)
    }
    
    data <- tidyr::gather(data, Station, Q, -Type, -Date)
    
    p <- ggplot2::ggplot() +
        ggplot2::geom_line(data = data[data$Type != "Observations",], 
                           ggplot2::aes(x=Date, y=Q), 
                  color="grey80") +
        ggplot2::geom_line(data = data[data$Type == "Observations",], 
                           ggplot2::aes(x=Date, y=Q), 
                  color="red", 
                  size=1) +
        ggplot2::facet_wrap(~Station, scales="free_y") + 
        ggplot2::theme_bw() +
        ggplot2::ylab('Q m3/s') +
        ggplot2::ggtitle('Observations against Hydrostreamer discharge estimate')
    
    print(p)
    return(p)
}



