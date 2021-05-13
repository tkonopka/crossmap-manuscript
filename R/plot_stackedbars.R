# plot functions for displaying stacked bars (like pie charts, but with rectangles)


#' stacked barplot
#'
#' @param d data frame with columns for a categorical label and a value
#' @param value.col character, column in d containing values
#' @param factor.col character
#' @param labels character, text to show along x-axis
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param main character, label for whole title
#' @param ylim numeric, limits for y axis
#' @param show.xaxis logical, toggle visibility of x axis
#' @param show.yaxis logical, toggle visibility of y axis
#' @param legend.pos numeric of length 2, position of legend
#' @param Rcssclass character, style class
#' 
plot.stackedbars <- function(d, value.col="", factor.col="", labels="",
                            xlab="", ylab="", main="", ylim=NULL,
                            value.template="FACTOR\nVALUE (PERCENT%)",
                            show.xaxis=TRUE, show.yaxis=FALSE,
                            legend.pos=NULL,
                            Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("stackedbars", Rcssclass))
  barwidth <- RcssValue("stackedbars", "barwidth", default=0.8)
  padding <- RcssValue("stackedbars", "padding", default=0.05)
  bw2 <- barwidth / 2
 
  xlim <- c(0.5, length(value.col)+0.5)
  d <- as.data.frame(d)
  
  if (is.null(ylim)) {
    ymax <- max(colSums(d[, value.col, drop=FALSE]))
    ylim <- c(0, ymax)
  }

  parplot(xlim, ylim, xlim=xlim, ylim=ylim, type="n", axes=FALSE, frame=FALSE)

  factor.colors <- c()
  for (.factor in unique(d[[factor.col]])) {
    factor.colors[.factor] <- RcssValue("rect", "col", default="#000000",
                                       Rcssclass=.factor)
  }
  
  for (i in seq_along(value.col)) {
    i.col <- value.col[i]
    y <- 0
    imax <- sum(d[[i.col]])
    for (j in seq_len(nrow(d))) {
      j.val <- d[[i.col]][j]
      j.perc <- round(100 * j.val / imax)
      j.factor <- d[[factor.col]][j]
      j.label <- gsub("FACTOR", j.factor, value.template)
      j.label <- gsub("VALUE", j.val, j.label)
      j.label <- gsub("PERCENT", j.perc, j.label)
      rect(i-bw2, y, i+bw2, y+j.val, col=factor.colors[j.factor],
           Rcssclass=j.factor)
      text(i, y+(j.val/2), j.label, Rcssclass="value")
      y <- y + j.val
    }
  }

  if (!is.null(legend.pos)) {
    legend(legend.pos[1], legend.pos[2],
           names(factor.colors), fill=factor.colors)
  }
  
  if (show.xaxis) {
    text(seq_along(labels), -padding*ylim[2], labels, Rcssclass="x")
  }
  if (show.yaxis) {
    axis(2, at=ylim, labels=c("", ""), line=0, tck=0, Rcssclass="y")
    axis(2, labels=NA, line=0, Rcssclass="y")
  }
  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=2, ylab, Rcssclass="ylab")
  mtext(side=3, main, Rcssclass="main")
}

