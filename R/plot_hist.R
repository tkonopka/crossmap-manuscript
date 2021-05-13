# plot functions for histograms


#' distribution summary using histogram
#'
#' @param v numeric vector
#' @param breaks numeric vector with histogram breakpoints
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param xlim numeric of length 2, limits for x-axis
#' @param main character, label for main title
#' @param Rcssclass character, style class
#'
plot.hist <- function(v,
                     breaks=seq(0, max(v)+1)-0.5,
                     xlim=c(-0.5, max(v)+0.5), ylim=NULL,
                     xlab="", ylab="", main="",
                     Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("hist",Rcssclass))
  par()
  if (is.null(ylim)) {
    vhist <- hist(v, breaks=breaks, plot=TRUE, xlim=xlim)
  } else {
    vhist <- hist(v, breaks=breaks, plot=TRUE, xlim=xlim, ylim=ylim)
  }
  # decorations
  axis(1, at=vhist$mid, label=vhist$mid, Rcssclass="x")
  axis(2, Rcssclass="y")
  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=2, ylab, Rcssclass="ylab")
  mtext(side=3, main, Rcssclass="main")
  invisible(vhist)
}


