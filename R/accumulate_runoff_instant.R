#' Apply instantaneous river routing
#' 
#' Applies the simplest possible river routing scheme, instantaenous flow, by 
#' adding runoff from each river segment to all of the segments downstream, for 
#' each timestep.
#'
#' @param HS A \code{HS} object.
#' @param verbose Whether to print progress indication or not. 
#'
#' @return Returns the input object \code{HS}) with an added list column
#'   \code{discharge_ts} containing routed discharge estimates for each river
#'    segment. 
#'    
#' @export
accumulate_runoff_instant <- function(HS, 
                                      verbose=FALSE) {
  
  riverID <- NULL
  UP_SEGMENTS <- NULL
  NEXT <- NULL
  
  lengths <- sf::st_length(HS) %>% unclass()
  IDs <- dplyr::select(HS, riverID) %>% 
    sf::st_set_geometry(NULL) %>% 
    unlist()
  
  order <- HS %>%
    dplyr::select(riverID, UP_SEGMENTS) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::arrange(UP_SEGMENTS) %>%
    dplyr::select(riverID) %>%
    unlist() %>%
    match(IDs)
  
  ## find next river
  ind <- find_attribute(HS, "next_col", TRUE)
  nextriver <- dplyr::pull(HS, ind) %>%
    match(IDs)
  
  
  discharge <- HS$runoff_ts
  
  # process all of downscaled runoff
  total <- length(order)
  if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
  prog <- 0
  for (seg in order) {
    # progress ind
    prog <- prog + 1
    
    # check and apply controls condition
    if (hasName(HS, "control_ts")) {
      if(!is.null(HS$control_ts[[seg]])) {
        control_ts <- HS$control_ts[[seg]]
        type <- HS$control_type[[seg]]
        dateind <- discharge[[seg]]$Date %in% control_ts$Date
        
        
        # Set, of modify input runoff of the segment
        if (type == "set") {
          for(pred in 2:ncol(discharge[[seg]])) {
            discharge[[seg]][dateind,pred] <- control_ts[,2]
          }
          
          # if no downstream segments, go to next seg
          if(!is.na(nextriver[[seg]])) {
            new_dis <- discharge[[nextriver[seg] ]][,-1] + 
              discharge[[seg]][,-1]
            
            discharge[[ nextriver[seg] ]][,-1] <- new_dis
          }
          next
        } else if (type == "add") {
          for(pred in 2:ncol(discharge[[seg]])) {
            discharge[[seg]][dateind,pred] <- 
              discharge[[seg]][dateind,pred] + control_ts[,2]
          }
        } else if (type == "subtract") {
          for(pred in 2:ncol(discharge[[seg]])) {
            discharge[[seg]][dateind,pred] <- 
              discharge[[seg]][dateind,pred] - control_ts[,2]
          }
        } else if (type == "multiply") {
          for(pred in 2:ncol(discharge[[seg]])) {
            discharge[[seg]][dateind,pred] <- 
              discharge[[seg]][dateind,pred] * control_ts[,2]
          }
          
        }
        
      }
    }
    
    # if there is no downstream segments, go to next seg
    if(is.na(nextriver[[seg]])) {
      next
    }
    
    # update next segment discharge
    new_dis <- discharge[[ nextriver[seg] ]][,-1] + discharge[[seg]][,-1]
    discharge[[ nextriver[seg] ]][,-1] <- new_dis
    
    #update progressbar
    if (verbose) setTxtProgressBar(pb, prog)
  }
  
  output <- HS 
  output$discharge_ts <- discharge
  output <- output %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()
  
  if (verbose) close(pb)
  
  output <- reorder_cols(output)
  output <- assign_class(output, "HS")
  return(output)
}
