MC_parameters <- function(inflow, 
                          seg,
                          compute_qref, 
                          q_ref,
                          compute_width,
                          channel_width,
                          width,
                          manning,
                          slope,
                          compute_celerity,
                          celerity) {
    
    # channel width
    if(compute_width) {
        
        # reference discharge
        if(compute_qref) {
            qb <- min(inflow, na.rm=TRUE)  
            qp <- max(inflow, na.rm=TRUE)
            qref <-  qb + 0.5*(qp-qb)
        } else {
            qref <- q_ref[seg]
        }
        
        w <- channel_width[1]*qref^channel_width[2]
    } else {
        w <- width[seg]
    }
    
    # channel depth parameters
    if(w < 10) {
        psi <- 1.064*w^0.928
        lambda <- 0.958
    } else {
        psi <- 0.274*w^1.608
        lambda <- 0.968
    }
    beta <- (5/3)*(1-(0.8*psi/(w^2+2*psi)))
    alfa <- (lambda*sqrt(slope[seg])*psi^((5/3)-beta)) /
        (manning[seg]*((w+2*psi)/w))^(2/3)
    
    if(compute_celerity) {
        d <- ((inflow/alfa)^(1/beta))/w
        
        # celerity
        A <- w*d
        Cel <- inflow/A
        # 
        # Cel <- compute_cel(inflow, alfa, beta, w) 
    } else {
        Cel <- celerity[seg]
    }
    Cel[is.na(Cel)] <- mean(Cel, na.rm=TRUE)
    
    # routing timestep (k-parameter)
    # dt <- lengths[seg] / Cel
    
    out <- list(qref = qref,
                w = w,
                # d = d,
                Cel = Cel)
                # dt = dt)
}

compute_cel <- function(inflow, alfa, beta, w) {
    d <- ((inflow/alfa)^(1/beta))/w
    
    # celerity
    A <- w*d
    Cel <- inflow/A
    
    return(Cel)
}


# make_inflow <- function(inflow, 
#                         n,
#                         weights,
#                         floor) {
#     
#     infl <- vector("numeric", n)
#     for(ii in seq_along(floor)) {
#         if(floor[ii] != 0) {
#             test <- floor[ii] == floor[ii-1]
#             
#             if(test) {
#                 # w <- weights[ii] - weights[ii-1]
#                 w <- weights[ii] - weights[ii - 1]
#                 infl[ floor[ii]+1 ] <- infl[ floor[ii]+1 ] +
#                     inflow[ii] * w
#                 
#             } else {
#                 n <- floor[ii] - floor[ii-1]
#                 
#                 if(n == 1) {
#                     w <- floor[ii] - weights[ii - 1]
#                     w2 <- weights[ii] - floor[ii]
#                     
#                     fl1 <- w * inflow[ ii ] 
#                     fl2 <- w2 * inflow[ ii ]
#                     
#                     infl[ floor[ii-1]+1 ] <- infl[ floor[ii-1]+1 ] + 
#                         fl1
#                     infl[ floor[ii] + 1 ] <- infl[ floor[ii] + 1 ] + 
#                         fl2
#                     
#                 } else { 
#                     w <- ceiling(weights[ii-1]) - weights[ii-1]
#                     w2 <- weights[ii] - floor[ii]
#                     
#                     fl1 <- w * inflow[ii] 
#                     fl2 <- w2 * inflow[ii]
#                     
#                     infl[ floor[ii-1]+1 ] <- infl[ floor[ii-1]+1 ] + 
#                         fl1
#                     infl[ floor[ii] + 1 ] <- infl[ floor[ii] + 1 ] + 
#                         fl2
#                     
#                     steps <- (floor[ii-1]+1):(floor[ii]-1) + 1
#                     infl[ steps ] <- infl[ steps ] + inflow[ii]                                 }
#             }
#             
#         } else {
#             if(ii == 1) {
#                 w <- weights[ii]
#                 infl[ floor[ii]+1 ] <- infl[ floor[ii]+1 ] + 
#                     inflow[ii] * w
#                 
#             } else {
#                 w <- weights[ii] - weights[ii-1]
#                 infl[ floor[ii]+1 ] <- infl[ floor[ii]+1 ] + 
#                     inflow[ii] * w
#             }
#         }
#     }
#     
#     return(infl)
#     
# }




# MUSKINGUM CUNGE ROUTING

# # for all timesteps, do muskingum
# for (t in 1:(length(outflow)-1)) {
# # for(t in 1:100) {
# 
#     C <- Cel_vec[t] * (3600/lengths[seg])
#     n_musk <- 1/C
#     routing_length <- lengths[seg] / n_musk
#     n <- floor(n_musk)
#     
#     C <- 1
#     D <- (infl[t]/w) / (slope[seg]*Cel_vec[t]*routing_length)
# 
#     X <- 0.5 * (1-D)
# 
#     m <- 1 + C + D
#     c0 <- (-1 + C + D) / m
#     c1 <- (1 + C - D) / m
#     c2 <- (1 - C + D) / m
# 
#     fl_out <- outflow[t]
#     fl_in <- infl[t]
#     
#     # route full lengths
#     if(n > 0) {
#         for(n in 1:n_musk) {
#             fl_out <- sum(c0 * infl[t+1],
#                       c1 * fl_in,
#                       c2 * fl_out,
#                       na.rm = TRUE)
#             
#             fl_in <- fl_out
#             
#         }
#     }
#     
#     # route fraction length
#     frac <- n_musk - n
#     routing_length <- routing_length * frac
#     dt <- 3600 * frac
#     
#     D <- (infl[t]/w) / (slope[seg]*Cel_vec[t]*routing_length)
#     
#     X <- 0.5 * (1-D)
#     
#     m <- 1 + C + D
#     c0 <- (-1 + C + D) / m
#     c1 <- (1 + C - D) / m
#     c2 <- (1 - C + D) / m
#     
#     fl_out <- sum(c0 * infl[t+1],
#               c1 * fl_in,
#               c2 * fl_out,
#               na.rm = TRUE)
#     
#     outflow[t+1] <- fl_out
#     
# 
# }

