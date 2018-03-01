








plot_flow <- function(station, rID, Q, obs, timeseries=NULL, ARCID = FALSE) {

  if (ARCID == FALSE) {
    Q.ts <- filter(Q, ID == rID) %>% select(-ID) %>% t() %>% unlist()
    #Q.ts <- select(Q, -ID)
  }
  if (ARCID == TRUE) {
    Q.ts <- filter(Q, ARCID == rID) %>% select(-ARCID) %>% t() %>% unlist()
    #Q.ts <- select(Q, -ARCID)
  }
  obs.ts <- select_(obs, station) %>% unlist()

  if( (length(Q.ts) == length(obs.ts))) {
    length.ts <- length(Q.ts)
  } else {
    shorter <- length(Q.ts) < length(obs.ts)
    if(shorter) {
      length.ts <- length(Q.ts)
      obs.ts <- obs.ts[1:length.ts]
    } else {
      length.ts <- length(obs.ts)
      Q.ts <- Q.ts[1:length.ts]
    }
  }


  p <- ggplot() +
    geom_point(aes(x = 1:length.ts, y=obs.ts)) +
    geom_point(aes(x = 1:length.ts, y=Q.ts)) +
    geom_line(aes(x=1:length.ts, y=obs.ts, colour='observed')) +
    geom_line(aes(x=1:length.ts, y=Q.ts, colour='downscaled flow')) +
    theme_bw() +
    ggtitle(paste0("Monthly average discharge @ HYMOS station ID: ", station)) +
    labs(y="Q (m3/s)", x = "Month")

  print(p)
  print(cor(Q.ts, obs.ts))
  #print(NSE(seg_ts, col_ts))
}





