#helper function to compute weights for line segments
compute_segment_weights <- function(segments, variable) {
    weights <- rep(0, length(segments))
    n <- sum(variable[segments])
    weights[segments] <- variable[segments]/n
    return(weights)
}



#helper function for delineate_basin
next_cell_up <- function(cell, drain.dir) {
    
    adj <- raster::adjacent(drain.dir, cell, directions=8, sorted=TRUE, pairs=FALSE)
    if (length(adj) < 8) return(NULL)
    dir <- drain.dir[adj]
    
    out <- 0
    
    if (dir[1] == 2) out <- c(out, adj[1])
    if (dir[2] == 4) out <- c(out, adj[2])
    if (dir[3] == 8) out <- c(out, adj[3])
    if (dir[4] == 1) out <- c(out, adj[4])
    if (dir[5] == 16) out <- c(out, adj[5])
    if (dir[6] == 128) out <- c(out, adj[6])
    if (dir[7] == 64) out <- c(out, adj[7])
    if (dir[8] == 32) out <- c(out, adj[8])
    
    if(length(out) == 1) return(NULL) else return(out[2:length(out)])
}



