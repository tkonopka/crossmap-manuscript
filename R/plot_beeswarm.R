# part of Crossmap analysis


#' draw a beeswarm plot 
#'
#' @param dlist list with numeric data
#' @param ylim numeric of length 2, limits for y-axis
#' @param main character, label for whole plot
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param labels named character vector, mapping from ids in dlist to text to show
#' to label each swarm
#' @param add.means logical, display bars with mean values for each group
#' @param Rcssclass character, style class
plot.beeswarm <- function(dlist, ylim=NULL,
                          main="", xlab="", ylab="",
                          labels=NULL,
                          add.means=TRUE,
                          Rcssclass=c()) {
  stop("deprecated")
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("beeswarm", Rcssclass))
  
  if (is.null(ylim)) {
    ylim <- c(0, max(unlist(dlist)))
  }
  xlim <- seq_along(dlist)+c(-0.5, 0.5)
  if (is.null(labels)) {
    labels <- setNames(names(dlist), names(dlist))
  }
  
  parplot(xlim, ylim, xlim=xlim, ylim=ylim)
  axis(1, at=seq_along(dlist), label=labels[names(dlist)], lwd=0, Rcssclass="x")
  axis(2, at=ylim, label=c("", ""), line=0, tck=0, Rcssclass="y")
  axis(2, lwd=0, Rcssclass="y")
  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=2, ylab, Rcssclass="ylab")
  mtext(side=3, main, Rcssclass="main")
  
  # compute and place the swarms
  spacing <- RcssValue("beeswarm", "spacing", default=2)
  corralWidth <- RcssValue("beeswarm", "corralWidth", default=1)
  pt.cex <- RcssValue("points", "cex", default=0.5)
  bee.original <- beeswarm(dlist, do.plot=FALSE, add=TRUE,
                           horizontal=FALSE,
                           cex=pt.cex, spacing=spacing,
                           corralWidth=corralWidth,
                           corral="random", method="swarm",
                           xlim=xlim, ylim=ylim)
  bee <- split(bee.original, bee.original$x.orig)
  meanbar.width <- min(1, corralWidth)/2
  for (groupname in names(bee)) {
    idata <- bee[[groupname]]
    group.x <- which(names(dlist)==groupname)
    idata.x <- mean(idata$x)
    group.mean <- mean(idata$y)
    if (add.means) {
      lines(group.x+c(-meanbar.width, meanbar.width), rep(group.mean, 2),
            Rcssclass=c("mean", groupname))
    }
    points(idata$x-idata.x+group.x, idata$y, Rcssclass=groupname)
  }

}

