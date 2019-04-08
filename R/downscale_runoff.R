#' Computes specific runoff generated in the river segment.
#' 
#' Computes downscaled, river segment specific, runoff using 
#' the given \code{HSweights} object. 
#'  
#' @param HSweights An object of class 'HSweights', obtained with 
#'   \code{compute_HSweights}, or constructed with function
#'   \code{create_HSweights}.
#' @param unit Unit of runoff. Can be either "mm/s", or "m3/s". 
#'   Defaults to mm/s (~ equivalent to kg/m2/s).
#' @param rID Name of the column in river with unique IDs.
#' @param wID Name of the column in weights with IDs corresponding to rID.
#' @param verbose Print progress indication or not.
#'
#' @return The routed river network object with class \code{HS}, which has been 
#'   enhanced with a runoff timeseries (list column \code{runoff_ts}. Runoff is 
#'   given in \eqn{m^3/s}.
#' 
#' @export
downscale_runoff <- function(HSweights, 
                             rID = "riverID", 
                             wID = "riverID", 
                             unit = "mm/s",
                             verbose = FALSE) {
    
    if(!any("HSweights" %in% class(HSweights))) {
        stop("Input should be of class HSweights.")
    }
    
    area_m2 <- NULL
    gridID <- NULL
    Date <- NULL
    riverID <- NULL
    
    river <- HSweights$river
    weights <- HSweights$weights
    grid <- HSweights$grid
    
    #output <- list(river = river, downscaled = list())
    
    
    
    nriv <- NROW(river)
    nseg <- NROW(weights)
    ng <- NROW(grid)
    
    rIDs <- dplyr::select_(river, rID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    gIDs <- dplyr::select(grid, gridID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    wrIDs <- dplyr::select_(weights, wID) %>% 
        sf::st_set_geometry(NULL) %>%
        unlist() %>%
        match(rIDs)
    wgIDs <- dplyr::select(weights, gridID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist() %>%
        match(gIDs)
    weightvec <- dplyr::select(weights, weights) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    gridareas <- dplyr::select(grid, area_m2) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    
    
    #runoff_ts <- vector("list", nrow(river))
    #runoff_ts <- lapply(runoff_ts, init_ts, grid, ngrids)
    
    runoff_ts <- collect_listc(grid$runoff_ts)
    ngrids <- length(runoff_ts)
    unidates <- lapply(grid$runoff_ts, function(x) x$Date) %>%
        unlist %>%
        unique %>%
        lubridate::as_date()
    
    total <- ngrids
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    qts <- list()
    for (g in 1:ngrids) {

        nts <- nrow(runoff_ts[[g]])

        # runoffTS <- dplyr::select(grid$runoff[[g]], -Date) %>%
        #     as.matrix()
        runoffTS <- runoff_ts[[g]]

        QTS <- matrix(0, nrow = nts, ncol = nriv)

        if (unit == "mm/s") convert <- TRUE
        if (unit == "m3/s") convert <- FALSE

        if(convert) {
            for (seg in 1:nseg) {
                QTS[, wrIDs[seg] ] <- QTS[, wrIDs[seg] ] + 
                    weightvec[seg] *
                    runoffTS[, wgIDs[seg] ] *
                    gridareas[ wgIDs[seg] ] / 1000

            }
        } else {
            for (seg in 1:nseg) {
                QTS[, wrIDs[seg] ] <- QTS[, wrIDs[seg] ] + 
                    weightvec[seg] *
                    runoffTS[, wgIDs[seg] ]
            }
        }


        QTS <- data.frame(QTS)
        colnames(QTS) <- rIDs
        QTS$Date <- unidates
        QTS <- dplyr::select(QTS, Date, dplyr::everything()) 


        if(is.null(colnames(grid$runoff_ts[[1]]))) {
            qts[[g]] <- QTS
        } else {
            name <- colnames(grid$runoff_ts[[1]])[g+1]
            qts[[ name ]] <- QTS
        }
        
        if (verbose) setTxtProgressBar(pb, g)
    }
    if (verbose) close(pb)
    
    listc <- spread_listc(qts)
    listc <- listc[order(names(listc))]
    
    output <- river %>% 
        dplyr::arrange(riverID)
    output <- tibble::add_column(output, runoff_ts = listc) %>%
        tibble::as_tibble() %>%
        sf::st_as_sf()
    
    output <- reorder_cols(output)
    output <- assign_class(output, "HS")
    return(output)
} 

