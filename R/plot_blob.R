

#' create coordinates for a rounded polygon
#'
#' @param x numeric, x coordinates of polygon corners
#' @param y numeric, y coordinates of polygon corners
#' @param p numeric, proportion of segment that will be rounded
#' @param n integer, number of new edges per segment
#'
#' @return list with $x and $y containing a new polygon
rounded.coords <- function(x, y=NULL, p=0.25, n=3) {

  # allow inputs as a single list containing $x and $y
  if (is.null(y)) {
    y <- x$y
    x <- x$x
  }
  
  # rounding of a single corner (three points turn to several smooth ones)
  knots.t <- seq(0, 1, length=n+1)
  interpolated <- function(x, p) {
    x[1]+p*(x[2]-x[1])
  }
  rounded.corner <- function(x, y) {
    x1 <- seq(interpolated(x[1:2], 1-p), x[2], length=n+1)
    y1 <- seq(interpolated(y[1:2], 1-p), y[2], length=n+1)
    x2 <- seq(x[2], interpolated(x[2:3], p), length=n+1)
    y2 <- seq(y[2], interpolated(y[2:3], p), length=n+1)
    list(x = x1*(1-knots.t) + x2*knots.t,
         y = y1*(1-knots.t) + y2*knots.t)
  }
  
  # apply corner rounding all around the polygon
  len <- length(x)
  x <- c(x, x[1], x[2])
  y <- c(y, y[1], y[2])
  result <- list(x=list(), y=list())
  for (i in seq_len(len)) {
    i.segments <- rounded.corner(x[i:(i+2)], y[i:(i+2)])
    result$x[[i]] <- i.segments$x
    result$y[[i]] <- i.segments$y
  }  
  list(x=unlist(result$x), y=unlist(result$y))
}


#' draw a rounded polygon
#'
#' @param x numeric, x positions of polygon corners
#' @param y numeric, y positions of polygon corners
#' @param p numeric, fraction of segment length to curve on each side
#' @param n integer, number of segments to approximate each curve
#' @param ... other arguments, passed on to polygon()
roundedpolygon <- function(x, y=NULL, p=0.25, n=3, ...) {
  coords <- rounded.coords(x, y, p, n)
  polygon(coords$x, coords$y, ...)
}

