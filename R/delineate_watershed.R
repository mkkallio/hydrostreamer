#' Identify locations of raster cells which flow in to a river confluence point.
#'
#' The function identifies the coordinates of raster cells which are just upstream from river confluence
#' points, identified from a river network layer.
#'
#' @param drain.dir A RasterLayer object of drainage directions.
#' @inheritParams compute_weights
#'
#' @section Details:
#' The logic is such that it looks at the last node-pair of each segment in the river network. The angle
#' of the last node pair is computed, and the raster cell in the direction of the river segment is
#' identified.
#'
#' Note that the logic of the function requires that the river network is derived from the same (or at least
#' the same resolution) raster than the given drainage directions.
#'
#' @return Returns an 'sf' point layer of identified raster cell centers.
#' @export
#'
#'
river_outlets <- function(drain.dir,river) {

  # shorten rivers the last node pair.
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
  rp <- raster::cellFromXY(drain.dir, sf::st_coordinates(p))
  nc <- raster::ncol(drain.dir)

  pointcoords <- data.frame(row = NULL, col = NULL)
  for (point in 1:length(rp)) {
    prc <- raster::rowColFromCell(drain.dir, rp[point]) %>% as.data.frame()

    cell <- rp[point]
    if(!is.na(cell)) {
        bearing <- b[point]

        prep <- data.frame(row=0, col=0)
        if (bearing > -5 && bearing < 5) {
          prep$row <- prc$row-1
          prep$col <- prc$col
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 350 && bearing < 370) {
          prep$row <- prc$row-1
          prep$col <- prc$col
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 40 && bearing < 50) {
          prep$row <- prc$row-1
          prep$col <- prc$col+1
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 80 && bearing < 100) {
          prep$row <- prc$row
          prep$col <- prc$col+1
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 125 && bearing < 145) {
          prep$row <- prc$row+1
          prep$col <- prc$col+1
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 170 && bearing < 190) {
          prep$row <- prc$row+1
          prep$col <- prc$col
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }


        if (bearing > 215 && bearing < 235) {
          prep$row <- prc$row+1
          prep$col <- prc$col-1
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 260 && bearing < 280) {
          prep$row <- prc$row
          prep$col <- prc$col-1
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
          pointcoords <- rbind(pointcoords, coords)
        }

        if (bearing > 305 && bearing < 325) {
          prep$row <- prc$row-1
          prep$col <- prc$col-1
          cell <- raster::cellFromRowCol(drain.dir, prep$row, prep$col)
          coords <- raster::xyFromCell(drain.dir, cell)
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








#' Delineate basin areas from multiple input points
#'
#'
#' @param points An 'sf' point object, with locations of the basins to be delineated.
#' @param ID Name of the column in points with unique identifiers.
#' @inheritParams river_outlets
#'
#' @return Returns basins specific to the points given, so that downstream basins are cut when point location
#'   is met uptstream.
#'
#' @export
#'
#'
delineate_basin <- function(drain.dir, points, ID = "ID") {
    # cells of interest
    rp <- raster::cellFromXY(drain.dir, sf::st_coordinates(points))
    points$cell <- rp
    points <- points[!is.na(points$cell),]

    # extract ids
    ID <- dplyr::select_(points, ID) %>%
      sf::st_set_geometry(NULL) %>%
      unlist()


    # initiate logical vector (visitation)
    visited <- logical(length=raster::ncell(drain.dir))
    visited[points$cell] <- TRUE
    #create empty raster
    basins <- drain.dir
    raster::values(basins) <- 0
    raster::values(basins)[points$cell] <- ID

    # number of columns and cells
    rdims <- dim(drain.dir)


    total <- raster::ncell(drain.dir)
    # create progress bar
    pb <- txtProgressBar(min = 0, max = total, style = 3)

    prcells <- 1:total
    prcells <- prcells[!is.na(raster::values(drain.dir))]
    # go through every cell
    for (cell in prcells) {

        cv <- logical(length = raster::ncell(drain.dir))
        curvisit <- visited[cell]
        current <- cell
        crcell <- raster::rowColFromCell(drain.dir,cell)

        # IF the current cell has NOT been visited
        if(!curvisit) {
            out <- FALSE
            while(curvisit == FALSE) {
                #information on current cell
                cv[current] <- TRUE
                direction <- drain.dir[current]



                # check that direction is okay (do not go outside of raster), and check if the next cell has been visited
                check <- check_bounds(crcell, rdims, direction)
                # if check = TRUE (we're good to go), find the next cell. else, break while loop
                if(check == TRUE) {
                    out <- FALSE
                    crcell <- next_cell(crcell, direction)
                    current <- raster::cellFromRowCol(drain.dir, crcell[1], crcell[2])
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
        raster::values(basins)[cv] <- basin_id
        visited[cv] <- TRUE
        setTxtProgressBar(pb, cell)
    }
    close(pb)

    return(basins)
}



# helper function which checks that drain.direction does not go out of bounds of the raster. If it does, return FALSE, if it doesnt, return TRUE
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


# helper function which identifies next cell from drainage direction
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



