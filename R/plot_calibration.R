# plots for calibrations


#' lines with dots for parameter calibration
#'
#' @param d data table
#' @param x character, column for x-axis
#' @param y character vector, columns for y-axis
#' @param show.points logical, display points as markers on top of lines
#' @param show.labels logical, show line labels on the right (labels come from y)
#' @param show.values character, determines if values for certain points are drawn
#' next to points
#' @param show.digits integer, number of significant digits to display
#' @param show.axes character, determines which axes are displayed
#' @param bg.stripe numeric of length 2, limits for y-axis for a stripe of
#' background color
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param main character, label for main title
#' @param xlim numeric of length 2, limits for x-axis
#' @param ylim numeric of length 2, limits for y-axis
#' @param Rcssclass character, style class
#'
plot.calibration.lines <- function(d, x, y=c("precision", "precision_bestN"),
                                   show.points=TRUE, show.labels=TRUE,
                                   show.values = c("min", "max", "first"),
                                   show.digits = 3,
                                   show.axes = c("x", "y"),
                                   bg.stripe = NA,
                                   xlab="", ylab="", main="",
                                   xlim=NULL, ylim=NULL,
                                   Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("caliblines", Rcssclass))
  
  if (is.null(names(y))) {
    y <- setNames(y, y)
  }
  if (is.null(xlim)) {
    xlim <- range(d[[x]])
  }
  if (is.null(ylim)) {
    ylim <- range(unlist(d[, names(y), with=FALSE]))
  }

  d <- d[order(d[[x]]),]
  parplot(xlim, ylim, type="n")
  if (!identical(bg.stripe, NA)) {
    xusr <- graphics::par()$usr[1:2]
    rect(xusr[1], bg.stripe[1], xusr[2], bg.stripe[2],
         Rcssclass="stripe")
  }

  ndigits <- show.digits
  for (.y in names(y)) {
    lines(d[[x]], d[[.y]], Rcssclass=.y)
    if (show.points) {
      points(d[[x]], d[[.y]], Rcssclass=.y)
    }
    if ("min" %in% show.values) {
      show.which <- which.min(d[[.y]])
      show.val = d[[.y]][show.which]
      text(d[[x]][show.which], show.val, signif(show.val, ndigits), Rcssclass="min")
    }
    if ("max" %in% show.values) {
      show.which <- which.max(d[[.y]])
      show.val <- d[[.y]][show.which]
      text(d[[x]][show.which], show.val, signif(show.val, ndigits), Rcssclass="max")
    }
    if ("first" %in% show.values) {
      show.val <- d[[.y]][1]
      text(d[[x]][1], show.val, signif(show.val, ndigits), Rcssclass="first")
    }
  }
  if (show.labels) {
    dmax <- d[which.max(d[[x]]),]
    xmax <- graphics::par()$usr[2]
    for (.y in names(y)) {
      text(xmax, dmax[[.y]], y[.y], Rcssclass=c("label", .y))
    }
  }
  
  # decorations (axes, axes labels)
  decorations.calibration(xlab, ylab, main, xlim, ylim,
                          show.axes=show.axes)
}



#' heatmap for two-parameter calibration
#'
#' @param d data table
#' @param xy character, columns for x-axis and y-axis
#' @param z character, column for z-axis, i.e. heatmap color
#' @param color.fun function, conversion from value to a color, e.g. viridis() 
#' @param show.values character, determines if values for min/max are shown
#' in the heatmap cell
#' @param show.digits integer, number of significant digits to display
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param main character, label for main title
#' @param xlim numeric of length 2, limits for x-axis
#' @param ylim numeric of length 2, limits for y-axis
#' @param Rcssclass character, style class
#'
plot.calibration.heat <- function(d, xy, z="precision", color.fun=viridis,
                                  show.values = c("min", "max"), show.digits=3,
                                  xlab="", ylab="", main="",
                                  xlim=NULL, ylim=NULL, zlim=NULL,
                                  Rcssclass=c()) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("calibheat", Rcssclass))

  if (is.null(names(z))) {
    z <- setNames(z, z)
  }
  zcolumn <- names(z)[1]
  zlabel <- as.character(z[1])

  if (is.null(xlim)) {
    xlim <- range(d[[xy[1]]])
  }
  if (is.null(ylim)) {
    ylim <- range(d[[xy[2]]])
  }
  if (is.null(zlim)) {
    zlim <- range(d[[zcolumn]])
  }
  d <- d[order(d[[xy[1]]], d[[xy[2]]]), ]
  
  parplot(xlim, ylim, type="n")
  
  get.intervals <- function(x) {
    x <- sort(unique(x))
    result <- cbind(x[-length(x)], x[-1])
    result <- unique(signif(result[,2]-result[,1], show.digits))
    if (length(result)>1) {
      stop("x and y values must be equally spaced")
    }
    result/2
  }
  x.interval <- get.intervals(d[[xy[1]]])
  y.interval <- get.intervals(d[[xy[2]]])
  
  # draw the heatmap
  dx <- d[[xy[1]]]
  dy <- d[[xy[2]]]
  dz <- d[[zcolumn]]
  dcol <- sapply((dz-zlim[1])/(zlim[2]-zlim[1]), function(w) {
    color.fun(1, begin=w, end=w)
  })
  rect(dx-x.interval, dy-y.interval, dx+x.interval, dy+y.interval,
       col=dcol, Rcssclass="cell")

  ndigits <- show.digits
  if ("min" %in% show.values) {
    show.which <- which.min(d[[zcolumn]])
    show.val <- d[[zcolumn]][show.which]
    text(d[[xy[1]]][show.which], d[[xy[2]]][show.which],
         signif(show.val, ndigits), Rcssclass=c("min", zcolumn))
  }
  if ("max" %in% show.values) {
    show.which <- which.max(d[[zcolumn]])
    show.val <- d[[zcolumn]][show.which]
    text(d[[xy[1]]][show.which], d[[xy[2]]][show.which],
         signif(show.val, ndigits), Rcssclass=c("max", zcolumn))
  }
  if ("first" %in% show.values) {
    show.val <- d[[zcolumn]][1]
    text(d[[xy[1]]][1], d[[xy[2]]][1],
         signif(show.val, ndigits), Rcssclass=c("first", zcolumn))
  }
  
  decorations.calibration(xlab, ylab, main, xlim, ylim)

  # draw the legend
  l.height <- RcssValue("calibheat", "legend.height", default=0.5)
  l.width <- RcssValue("calibheat", "legend.width", default=0.05)
  l.pad <- RcssValue("calibheat", "legend.padding", default=0.05)
  l.res <- RcssValue("calibheat", "legend.resolution", default=64)
  l.text <- RcssValue("calibheat", "legend.textpad", default=0.02)
  legend.x <- xlim[2] + (l.pad + c(0, l.width, l.width+l.text))*(xlim[2]-xlim[1])
  legend.y <- ylim[1] + c(l.height/2, 1-l.height/2)*(ylim[2]-ylim[1])
  legend.scale <- seq(legend.y[1], legend.y[2], length=l.res+1)
  
  legend.colors <- color.fun(l.res, begin=0, end=1)
  rect(legend.x[1], legend.scale[-length(legend.scale)],
       legend.x[2], legend.scale[-1], col=legend.colors,
       Rcssclass=c("legend", "inner"))
  rect(legend.x[1], legend.y[1], legend.x[2], legend.y[2],
       Rcssclass=c("legend", "outer"))
  text(legend.x[3], c(legend.y[1], mean(legend.y), legend.y[2]),
       signif(c(zlim[1], mean(zlim), zlim[2]), ndigits),
       Rcssclass="legend")
}


#' add decorations to a plot
#'
#' @param xlab character
#' @param ylab character
#' @param main character
#' @param xlim numeric of length 2
#' @param ylim numeric of length 2
#' @param show.axes character
#' 
decorations.calibration <- function(xlab, ylab, main, xlim, ylim,
                                    show.axes=c("x", "y")) {
  # decorations
  if ("x" %in% show.axes) {
    axis(1, at=xlim, labels=c("", ""), line=0, tck=0, Rcssclass="x")
    axis(1, labels=NA, line=0, Rcssclass="x")
    axis(1, lwd=0, Rcssclass="x")
    mtext(side=1, xlab, Rcssclass="xlab")
  }
  if ("y" %in% show.axes) {
    axis(2, at=ylim, labels=c("", ""), line=0, tck=0, Rcssclass="y")
    axis(2, labels=NA, line=0, Rcssclass="x")
    axis(2, lwd=0, Rcssclass="y")
    mtext(side=2, ylab, Rcssclass="ylab")
  }
  mtext(side=3, main, Rcssclass="main")
}

