#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;


List rlecpp(NumericVector x) {
    std::vector<int> lengths;
    std::vector<double> values;
    
    // Initialise first value
    int i = 0;
    double prev = x[0];
    values.push_back(prev);
    lengths.push_back(1);
    
    for(NumericVector::iterator it = x.begin() + 1; it != x.end(); ++it) {
        if (prev == *it) {
            lengths[i]++;
        } else {
            values.push_back(*it);
            lengths.push_back(1);
            
            i++;
            prev = *it;
        }
    }
    
    return List::create(_["lengths"] = lengths, _["values"] = values);
}


//[[Rcpp::export]]
NumericMatrix constantroute(NumericMatrix inflow, 
                            List record,
                            int pad_n,
                            int nseg) {
    NumericMatrix outflow(inflow.nrow(), inflow.ncol());
    int tstart = pad_n;
    int tend = inflow.nrow() - pad_n-1;
    List segdata;
    
    for (int seg = 0; seg < nseg; seg++) {
        for (int ts = tstart; ts < tend; ts++) {
            segdata = record[seg];
            IntegerVector tsteps = segdata[1];
            IntegerVector segments = segdata[2];
            NumericVector shares = segdata[0];
            int tsi = ts-pad_n;
            
            //double current = outflow(tsi, seg);
            double flow = 0;
            int n = tsteps.size();
            
            for (int i = 0; i < n; i++) {
                int tsc = ts-tsteps(i);
                int segm = segments(i)-1;
                double shr = shares(i);
                double inf = inflow(tsc, segm);
                flow = flow + inf * shr;
            }
            
            outflow(tsi, seg) = flow;
        }
    }
    
    return outflow;
}

/*
NumericMatrix routereverse(NumericVector ds_cumulative,
                           List ds_inds,
                           NumericVector intervals,
                           NumericMatrix inflow,
                           NumericMatrix flowts,
                           IntegerVector ts_loc) {
    
    // helper variables
    int tsteps = inflow.nrow();
    int nseg = inflow.ncol();
    
    // record
    List stat_out;
    
    // the output
    NumericMatrix outflow(tsteps, nseg);
    
    for (int seg = 0; seg < nseg; seg++) {
        IntegerVector ds = ds_inds[seg];
        
        for(int ts = 0; ts < tsteps; ts++) {
            double q = inflow(ts, seg); 
            
            NumericVector cumulative = ds_cumulative / intervals[ts];
            
            NumericVector tf = floor(cumulative);
            NumericVector tfuni = unique(tf);
            
            List rle = rlecpp(tf);
            IntegerVector last = cumsum(rle["length"]);
            last.push_front(0);
            
            NumericMatrix out(tfuni.size() +1, cumulative.size());
            
            // comp flow
            for(int i = 1; i < last.size(); i++) {
                int f = last(i-1)+1;
                int l = last(i);
                
                //outflow = q * how long it takes to pass through during
                //the timestep
                outflow(i-1, Range(f,l)) = q*(1-(cumulative(Range(f,l))-tf(Range(f,l))));
            
                //# flow that did not flow through the segments during,
                //# the timestep, flows through on the next tstep (?)
                outflow(i, Range(f,l)) = q-outflow(i-1, Range(f,l));

            }
            
            // record contribution at the station
            for(int i = 1; i < last.size(); i++) {
                int last_col = outflow.ncol();
                
                NumericVector flowstat = outflow(_, last_col);
                NumericVector flowcontrib;
                IntegerVector tss;
                for(int ii = 0; ii < flowstat.size(); ii++) {
                    if(flowcontrib(ii) != 0) {
                        tss.push_front(ts-(ii-1));
                        flowcontrib.push_front(flowstat(ii));
                    }
                }
                List record = List::create(Named("tss") = tss , _["flow"] = flowcontrib);
                stat_out.push_back(record);
            }
        }
    }
    
    
    return(outflow);
    
}
*/

// record_reverse <- function(downstream, downstreamind, interval, inflowmat) {
//     
//     record <- lapply(1:nseg, function(x) {
//         list(shares = NULL,
//              tsteps = NULL,
//              segment = NULL)
//     })
//     interval <- mean(interval)
//     for(seg in 1:nseg) {
//         
//         
//         # get indices of downstream segments
//         ds <- downstreamind[[seg]]
//         
//         # inflowing runoff at segment seg, timestep ts
//         q <- 1
//         
//         # cumulative time through all downstream segments standardized by
//         # the interval between timesteps
//         cumulative <- downstream[[seg]]$cumulative / interval
//         tf <- floor(cumulative)
//         tfuni <- unique(tf)
//             
//         # Identify last segment water flows through in each segment
//         last <- c(0, cumsum(rle(tf)$lengths))
//             
//         outflow <- matrix(0,
//                               nrow = length(tfuni)+1,
//                               ncol = length(cumulative))
//                 
//         for(i in 2:length(last)) {
//         # first and last segment
//             f <- last[i-1]+1
//             l <- last[i]
//             
//             # outflow = q * how long it takes to pass through during
//             # the timestep
//             outflow[i-1,f:l] <- q*(1-(cumulative[f:l]-tf[f:l]))
//                 
//             # flow that did not flow through the segments during,
//             # the timestep, flows through on the next tstep (?)
//             outflow[i,f:l] <- q-outflow[i-1,f:l]
//         }
//                 
//         for(i in 1:ncol(outflow)) {
//             shares <- outflow[,i]
//             tsteps <- which(shares != 0)
//             shares <- shares[tsteps]
//             segment <- ds[i]
//             rec <- record[[segment]]
//             rec$shares <- c(rec$shares, rev(shares))
//             rec$tsteps <- c(rec$tsteps, as.integer(rev(tsteps-1)))
//             rec$segment <- c(rec$segment, rep(seg, length(shares)))
//             record[[segment]] <- rec
//         }
//     }
//     return(record)
// }