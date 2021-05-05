#' Evaluates applicability of instantaneous flow routing
#' 
#' Function to evaluate whether instantaneous flow routing may be used for
#' a certain river network and a specified timestep length. 
#' 
#' @param HS a \code{HS} object obtained passed through
#'    \code{\link{river_network}}.
#' @param timestep Timestep length in \code{seconds}.
#' @param velocity Constant flow velocity in \code{meters per second}.
#' @inheritParams compute_HSweights
#' 
#' @return A vector of the collected stats, or the input \code{HS} with a 
#'   specified column storing the vector. 
#' 
#' @export
evaluate_instant_routing <- function(HS, 
                                     timestep,
                                     velocity = 1,
                                     verbose = FALSE) {
    
    # --------------------------------------------------------------------------
    # test input
    
    test <- inherits(HS, "HS")
    if(!test) stop("HS input should be of class 'HS'.")
    
    net_len <- units::drop_units(compute_network_length(HS, 
                                                        sf::st_length(HS),
                                                        verbose = FALSE))
    
    # --------------------------------------------------------------------------
    # process
    
    Mmax <- max(net_len, na.rm=TRUE) / velocity / timestep
    Mmean <- mean(net_len, na.rm=TRUE) / velocity / timestep
    
    if(verbose) {
        message("Maximum flow time through the network is ", round(Mmax*100,2), 
                " % of time timestep length. \n",
                "Mean flow time through the network is ", round(Mmean*100,2),
                " % of the timestep length.")
    }
    
    return(list(M_max = Mmax,
                M_mean = Mmean))
    
}
