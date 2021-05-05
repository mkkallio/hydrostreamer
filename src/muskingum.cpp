#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector do_musk(NumericVector infl,
                      NumericVector outfl,
                      NumericVector cel,
                      double w,
                      double s,
                      double l) {
    
    int n = infl.size(); 
    
    for(int i = 0; i < n-1; i++) {
        
        double C = cel[i] * (3600 / l);
        double n_musk = 1/C;
        double routing_length = l/n_musk;
        int times = floor(n_musk);
        
        C = 1;
        double D = (infl[i] / w) / (s * cel[i] * routing_length);
        
        //double X = 0.5 * (1-D);
        
        double m = 1 + C + D;
        double c0 = (-1 + C + D) / m;
        double c1 = (1 + C - D) / m;
        double c2 = (1 - C + D) / m;
        
        double fl_out = outfl[i];
        double fl_in = infl[i];
        
        // route full lengths
        if(times > 0) {
          
          for(int ii = 0; ii < times; ii++) {
            fl_out = (c0 * infl[i+1]) + (c1 * fl_in) + (c2 * fl_out);
            fl_in = fl_out;
          }
        }
        
        double frac = n_musk - floor(n_musk);
        routing_length = routing_length * frac;
          
        D = (infl[i] / w) / (s * cel[i] * routing_length);
          
        m = 1 + C + D;
        c0 = (-1 + C + D) / m;
        c1 = (1 + C - D) / m;
        c2 = (1 - C + D) / m;
        
        fl_out = (c0 * infl[i+1]) + (c1 * fl_in) + (c2 * fl_out);
        
        outfl[i+1] = fl_out;
    }
    
    return(outfl);
  
}


// 
// for (t in 1:(length(outfl)-1)) {
// # for(t in 1:100) {
//   
  // C <- Cel_vec[t] * (3600/lengths[seg])
  // n_musk <- 1/C
  // routing_length <- lengths[seg] / n_musk
  // n <- floor(n_musk)
  // 
  // C <- 1
  // D <- (infl[t]/w) / (slope[seg]*Cel_vec[t]*routing_length)
  // 
  // X <- 0.5 * (1-D)
  // 
  // m <- 1 + C + D
  // c0 <- (-1 + C + D) / m
  // c1 <- (1 + C - D) / m
  // c2 <- (1 - C + D) / m
  // 
  // fl_out <- outfl[t]
  // fl_in <- infl[t]
//   
// # route full lengths
//   if(n > 0) {
//     for(n in 1:n_musk) {
//       fl_out <- sum(c0 * infl[t+1],
//                     c1 * fl_in,
//                     c2 * fl_out,
//                     na.rm = TRUE)
//       
//       fl_in <- fl_out
//       
//     }
//   }
//   
// # route fraction length
//   frac <- n_musk - n
//     routing_length <- routing_length * frac
//     dt <- 3600 * frac
//     
//     D <- (infl[t]/w) / (slope[seg]*Cel_vec[t]*routing_length)
//     
//     X <- 0.5 * (1-D)
//     
//     m <- 1 + C + D
//     c0 <- (-1 + C + D) / m
//     c1 <- (1 + C - D) / m
//     c2 <- (1 - C + D) / m
//     
//     fl_out <- sum(c0 * infl[t+1],
//                   c1 * fl_in,
//                   c2 * fl_out,
//                   na.rm = TRUE)
//     
//     outfl[t+1] <- fl_out
//     
//     
// }