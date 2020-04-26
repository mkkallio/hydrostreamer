//#include <Rcpp.h>
//using namespace Rcpp;
/*
// [[Rcpp::export]]
NumericVector fix_error(NumericVector max_err, NumericVector new_err,  int n) {
    
    bool test;
    double dist_err;
    double fix_err;
    int npix;
    Rcpp::NumericVector fixed_err(n);
    double total_dist = 0;
    
    for (int i = 0; i < n; i++){
        test = abs(new_err[i] + total_dist) > abs(max_err[i]);
        if(test) {
            bool pos = new_err[i] + total_dist >= 0;
            
            if(pos) {
                dist_err = new_err[i] + total_dist - max_err[i];
                fix_err = max_err[i];
            }  else {
                dist_err = new_err[i] + total_dist + max_err[i];
                fix_err = -max_err[i];
            }
            
            npix = n-(i+1);
            dist_err = dist_err/npix;
            total_dist = total_dist + dist_err;
            
            //for(int ii = i+1; ii < n; ii++) {
                //	new_err(ii) = new_err(ii) + dist_err;
                //}
            
            fixed_err[i] = fix_err;
        } else {
            fixed_err[i] = new_err[i] + total_dist;
        }
    }
    return fixed_err;
}


// [[Rcpp::export]]
NumericVector iterate_pp(List p_obj, 
                         NumericVector dasy, 
                         List touching,
                         List boundary,
                         NumericVector r_grid,
                         NumericVector gridareas,
                         int n,
                         bool convert) { 
     
     // helping variables
     int size = p_obj.size()
    
}

iterate_pycno <- function(p_obj, dasy = NULL, touching, boundary, 
                          r_grid, gridareas, n, convert=TRUE) {
    
    # prepare
    r_orig <- rep(NA, length(touching))
    for(i in seq_along(r_grid)) {
        ind <- which(p_obj$zoneID == as.numeric(names(r_grid)[i]))
        r_orig[ind] <- r_grid[[i]]
    }
    r_prev <- r_orig
    r_curr <- r_orig
    
    iter <- which(!is.na(p_obj$riverID))
    remove <- which(is.na(p_obj$riverID))
    p_obj$zoneID[remove] <- NA
    
    # iterate
    for (i in 1:n) {
        
        for(j in iter) {
            ind <- c(j, touching[[j]])
            boundary_val <- r_orig[j] * boundary[j]
            vals <- c(r_prev[ind], boundary_val)
            new_value <- mean(vals, na.rm=TRUE)
            r_curr[j] <- new_value
        }
        
        # # rescale
        for(j in seq_along(r_grid)) {
            ind <- which(p_obj$zoneID == j)
            vol_c <- r_curr[ind] * p_obj$b_area_m2[ind]
            vol_g <- r_grid[j]*gridareas[j]
            bias <- vol_g / sum(vol_c,na.rm = TRUE)
            vol_c <- vol_c * bias
            
            r_curr[ind] <- vol_c / p_obj$b_area_m2[ind]
        }
        
        r_prev <- r_curr    
    }
    
    # if dasymetric variable is provided
    if(!is.null(dasy)) {
        r_curr <- r_curr * dasy
        
        # same as above
        for(j in seq_along(r_grid)) {
            ind <- which(p_obj$zoneID == j)
            vol_c <- r_curr[ind] * p_obj$b_area_m2[ind]
            vol_g <- r_grid[j]*gridareas[j]
            bias <- vol_g / sum(vol_c,na.rm = TRUE)
            vol_c <- vol_c * bias
            
            r_curr[ind] <- vol_c / p_obj$b_area_m2[ind]
        }
    }
    
    return(r_curr)
} 
*/