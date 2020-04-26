#' Compute weights for river catchment areas within the runoff area 
#' features. 
#' 
#' Computes weights for each individual river segment specific catchments 
#' falling in the areal units of the runoff \emph{HS}. Function first 
#' takes a union between \emph{basins} and \emph{HS} (creating new 
#' catchment units which fall inside only one runoff unit), and calculating 
#' the area for each individual catchment unit. The weight is assigned by 
#' dividing the area of sub-catchment with the area of runoff unit.
#' This function is called by \code{\link{compute_HSweights}}.
#'
#' @param basins An 'sf' polygon feature specifying the river segment 
#'   specific catchments.
#' @param riverID Column in \code{basins} containing unique IDs.
#' @param zoneID Column in  \code{HS} with unique IDs.
#' @inheritParams compute_HSweights
#'
#' @return Returns an 'sf' polygon feature (a union of basins, and HS) 
#'   with added attributes (columns):
#'   \itemize{
#'     \item \emph{ID}. Unique ID of the feature.
#'     \item \emph{riverID}. ID of the river segment each sub-catchment is 
#'       associated to.
#'     \item \emph{zoneID}. ID of the runoff unit the sub-catchment 
#'       is contained in.
#'     \item \emph{weights}. Weights computed for each sub-catchment.
#'     \item \emph{target_area}. Area of the sub-catchment (basin) in 
#'       \eqn{m^2}.
#'     \item \emph{source_area}. Area of the runoff unit sub-catchment is 
#'       contained in.
#' }
#' 
#' @export
compute_area_weights <- function(basins, 
                                 HS,
                                 pycno = NULL,
                                 dasy = NULL,
                                 weights = NULL,
                                 n = 20,
                                 intensive = TRUE,
                                 riverID = "riverID", 
                                 zoneID = "zoneID") {
    
    area <- NULL
    weights <- NULL
    ID <- NULL
    target_area <- NULL
    source_area <- NULL
    
    # TEST INPUTS
    #############
    
    if(!hasName(basins, riverID)) stop("riverID column '", 
                                       riverID, 
                                       "' does not exist in basins")
    if(!riverID == "riverID") basins <- dplyr::rename_(basins, 
                                                       riverID = riverID) 
    
    if(is.null(pycno)) {
        pycnophylactic <- FALSE
    } else {
        pycnophylactic <- TRUE
        test <- hasName(HS, pycno)
        if(!test) stop("No column ", pycno," in basins")
        test <- sum(is.null(HS[,pycno]))
        test2 <- sum(is.na(HS[,pycno]))
        if(test+test2 > 0) stop("Missing values in column ", pycno)
    }
    
    if(is.null(dasy)) {
        dasymetric <- FALSE
    } else {
        dasymetric <- TRUE
        test <- hasName(basins, dasy)
        if(!test) stop("No column ", dasy," in basins")
        test <- sum(is.null(basins[,dasy]))
        test2 <- sum(is.na(basins[,dasy]))
        if(test+test2 > 0) stop("Missing values in column ", dasy)
    }
    
    if(is.null(weights)) {
        user_weights <- FALSE 
    } else {
        user_weights <- TRUE
        test <- hasName(basins, weights)
        if(!test) stop("No column ", weights," in basins")
    }
    
    if (hasName(basins,"weights")) {
        warning("Replacing existing 'weights' column")
    }
    
    # PROCESS
    #########
    
    basins <- suppressWarnings(
        suppressMessages(
            HS %>% 
                dplyr::select(zoneID, source_area = area) %>%
                sf::st_intersection(basins, .)))
    
    basins <- sf::st_collection_extract(basins, "POLYGON") %>%
        tibble::add_column(target_area = sf::st_area(.)) %>%
        dplyr::filter(target_area != units::set_units(0, "m^2"))
    
    if(user_weights) {
        w <- dplyr::pull(basins, weights)
        basins <- basins %>% dplyr::mutate(basins, 
                                           weights = w) 
        
    } else if(pycnophylactic) {
        pycno_var <- dplyr::pull(HS, pycno)
        names(pycno_var) <- HS$zoneID
        
        #identify neighbours
        touching <- sf::st_touches(basins)
        
        #identify boundary
        boundary <- sf::st_touches(basins, 
                                   sf::st_union(basins) %>%
                                       sf::st_cast("POLYGON") %>%
                                       sf::st_cast("LINESTRING"),
                                   sparse = FALSE) %>%
            as.numeric()
        boundary[boundary == 0] <- NA
        
        gridareas <- HS$area_m2
        
        if(dasymetric) {
            dasymetric_var <- dplyr::pull(basins, dasy)
            pycno_res <- iterate_pycno(basins, 
                                       dasy = dasymetric_var,
                                       touching, 
                                       boundary,
                                       pycno_var, 
                                       gridareas, 
                                       n,
                                       convert=!intensive)
            
        } else {
            pycno_res <- iterate_pycno(basins, 
                                       dasy = NULL,
                                       touching, 
                                       boundary,
                                       pycno_var,
                                       gridareas, 
                                       n,
                                       convert=!intensive)
        }
        
        pycno_res <- pycno_res * units::drop_units(basins$target_area)
        
        
        basins <- basins %>%
            dplyr::mutate(weights = pycno_res) %>%
            dplyr::group_by(zoneID) %>%
            dplyr::mutate(sum = sum(weights),
                          weights = weights/sum) %>%
            dplyr::ungroup() %>%
            dplyr::select(-sum) 
        
    } else if(!pycnophylactic && dasymetric) { # replicated above
        dasymetric_var <- dplyr::pull(basins, dasy)
        
        basins <- basins %>%
            tibble::add_column(variable = dasymetric_var) %>%
            dplyr::group_by(zoneID) %>%
            dplyr::mutate(bas_dasy = variable*units::drop_units(target_area),
                          denom = sum(bas_dasy),
                          weights = bas_dasy/denom) %>%
            dplyr::ungroup() %>%
            dplyr::select(-variable, -bas_dasy, -denom)
        
    } else if(!pycnophylactic && !dasymetric) {
        basins <- dplyr::mutate(basins, 
                                weights = basins$target_area/basins$source_area) 
    } else {
        stop("error")
    }
    
    #reorder and add columns 
    if (!any(names(basins) == "ID")) basins$ID <- 1:nrow(basins)

    basins <- basins %>% dplyr::select(ID, riverID, zoneID, weights,
                                       target_area, source_area, 
                                       dplyr::everything())
    
    return(basins)
}
