# get points
process_junctions <- function(drdir,river) {

  # shorten rivers
  p4s <- sf::st_crs(river)
  for(line in 1:NROW(river)) {
    coords <- sf::st_coordinates(river[line,])
    len <- NROW(coords)
    coords <- coords[(len-1):len,1:2]
    if(line == 1) {
      p <- sf::st_point(coords[2,]) %>% sf::st_sfc()
      b <- geosphere::bearing(coords)
    } else {
      p2 <- sf::st_point(coords[2,]) %>% sf::st_sfc()
      p <- c(p, p2)
      b2 <- geosphere::bearing(coords)
      b <- c(b, b2)
    }
  }
  b <- b[!is.na(b)]
  b <- b+180
  p <- sf::st_set_crs(p, p4s)

  # cells of interest
  rp <- raster::cellFromXY(drdir, sf::st_coordinates(p))
  nc <- ncol(drdir)

  pointcoords <- data.frame(row = NULL, col = NULL)
  for (point in 1:length(rp)) {
    prc <- raster::rowColFromCell(drdir, rp[point]) %>% as.data.frame()

    cell <- rp[point]
    if(!is.na(cell)) {
        bearing <- b[point]

        prep <- data.frame(row=0, col=0)
        if (bearing > -5 && bearing < 5) {
          prep$row <- prc$row-1
          prep$col <- prc$col
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 350 && bearing < 370) {
          prep$row <- prc$row-1
          prep$col <- prc$col
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 40 && bearing < 50) {
          prep$row <- prc$row-1
          prep$col <- prc$col+1
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 80 && bearing < 100) {
          prep$row <- prc$row
          prep$col <- prc$col+1
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 125 && bearing < 145) {
          prep$row <- prc$row+1
          prep$col <- prc$col+1
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 170 && bearing < 190) {
          prep$row <- prc$row+1
          prep$col <- prc$col
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }


        if (bearing > 215 && bearing < 235) {
          prep$row <- prc$row+1
          prep$col <- prc$col-1
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 260 && bearing < 280) {
          prep$row <- prc$row
          prep$col <- prc$col-1
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 305 && bearing < 325) {
          prep$row <- prc$row-1
          prep$col <- prc$col-1
          cell <- raster::cellFromRowCol(drdir, prep$row, prep$col)
          coords <- raster::xyFromCell(drdir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }
      } else {
          coords <- c(NA,NA)
          pointcoords <- rbind(pointcoords, coords)
      }

  }

  #pointcoords <- matrix(pointcoords, ncol=2)
  points <- sf::st_multipoint(as.matrix(pointcoords)) %>%
      sf::st_sfc() %>%
      sf::st_cast("POINT") %>%
      sf::st_set_crs(p4s)

  sf::st_geometry(river) <- points
  empty <- sf::st_dimension(river)
  river <- river[!is.na(empty),]

  return(river)

}








# delineate basin
delineate_basin <- function(drdir, points, ID = "ID") {
    # cells of interest
    rp <- raster::cellFromXY(drdir, sf::st_coordinates(points))
    points$cell <- rp
    points <- points[!is.na(points$cell),]

    # extract ids
    ID <- dplyr::select_(points, ID) %>%
      sf::st_set_geometry(NULL) %>%
      unlist()


    # initiate logical vector (visitation)
    visited <- logical(length=ncell(drdir))
    visited[points$cell] <- TRUE
    #create empty raster
    basins <- drdir
    values(basins) <- 0
    values(basins)[points$cell] <- ID

    # number of columns and cells
    rdims <- dim(drdir)


    total <- ncell(drdir)
    # create progress bar
    pb <- txtProgressBar(min = 0, max = total, style = 3)

    prcells <- 1:total
    prcells <- prcells[!is.na(values(drdir))]
    # go through every cell
    for (cell in prcells) {

        cv <- logical(length = ncell(drdir))
        curvisit <- visited[cell]
        current <- cell
        crcell <- raster::rowColFromCell(drdir,cell)

        # IF the current cell has NOT been visited
        if(!curvisit) {
            out <- FALSE
            while(curvisit == FALSE) {
                #information on current cell
                cv[current] <- TRUE
                direction <- drdir[current]



                # check that direction is okay (do not go outside of raster), and check if the next cell has been visited
                check <- check_bounds(crcell, rdims, direction)
                # if check = TRUE (we're good to go), find the next cell. else, break while loop
                if(check == TRUE) {
                    out <- FALSE
                    crcell <- next_cell(crcell, direction)
                    current <- raster::cellFromRowCol(drdir, crcell[1], crcell[2])
                    curvisit <- visited[current]
                } else {
                  out <- TRUE
                  break
                }


                if(curvisit == TRUE) {break}
            }
        }
        if (out) {
          basin_id <- -9999
        } else {
          basin_id <- basins[current]
        }
        values(basins)[cv] <- basin_id
        visited[cv] <- TRUE
        setTxtProgressBar(pb, cell)
    }
    close(pb)

    return(basins)
}

#checks that drdirection does not go out. If it does, return FALSE, if it doesnt, return TRUE
# HELPER FOR delineate_basin()
check_bounds <- function(curcell, rdims, direction) {
  if (is.na(direction)) {
    return(FALSE)
  } else if (direction == 1 && rdims[2] == curcell[2]) {
    return(FALSE)
  } else if(direction == 2 && (rdims[2] == curcell[2] || rdims[1] == curcell[1])) {
    return(FALSE)
  } else if(direction == 4 && (rdims[1] == curcell[1])) {
    return(FALSE)
  } else if(direction == 8 && (rdims[1] == curcell[1] || curcell[2] == 1)) {
    return(FALSE)
  } else if(direction == 16 && (curcell[2] == 1)) {
    return(FALSE)
  } else if(direction == 32 && (curcell[1] == 1 || curcell[2] == 1)) {
    return(FALSE)
  } else if(direction == 64 && (curcell[1] == 1)) {
    return(FALSE)
  } else if(direction == 128 && (curcell[1] == 1 || rdims[2] == curcell[2])) {
    return(FALSE)
  } else if(direction == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#checks that drdirection does not go out. If it does, return FALSE, if it doesnt, return TRUE
# HELPER FOR delineate_basin()
next_cell <- function(crcell, direction) {
  newcell <- crcell
  if (direction == 1) {
    newcell[2] <- newcell[2]+1

  } else if(direction == 2) {
    newcell[1] <- newcell[1]+1
    newcell[2] <- newcell[2]+1

  } else if(direction == 4) {
    newcell[1] <- newcell[1]+1

  } else if(direction == 8) {
    newcell[1] <- newcell[1]+1
    newcell[2] <- newcell[2]-1

  } else if(direction == 16) {
    newcell[2] <- newcell[2]-1

  } else if(direction == 32) {
    newcell[1] <- newcell[1]-1
    newcell[2] <- newcell[2]-1

  } else if(direction == 64) {
    newcell[1] <- newcell[1]-1

  } else if(direction == 128) {
    newcell[1] <- newcell[1]-1
    newcell[2] <- newcell[2]+1
  }
  return(newcell)
}



