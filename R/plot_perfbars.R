# plot functions for displaying performance as vertical bars


#' distribution summaries using box-and-whiskers
#'
#' @param d data frame with columns for a categorical label and a value
#' @param label.col character, column in d containing a label
#' (for horizontal axis)
#' @param value.col character vector, columns in d containing values
#' @param q numeric of length 2, quantiles for errors bars
#' @param xlim numeric of length 2, limits for horizontal axis
#' @param ylim numeric of length 2, limits for vertical axis
#' @param xlab character, label for x-axis
#' @param main character, label for whole title
#' @param submain character, label just underneat title
#' @param show.yvals logical, whether to display number by the y-axis
#' @param Rcssclass character, style class
#' 
plot.perfbars <- function(d, label.col="", value.col="", labels=c("-", "+"),
                         q=c(0.25, 0.75),
                         xlim=NULL, ylim=NULL,
                         xlab="", main="", submain="", show.yvals=TRUE,
                         Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("perfbars", Rcssclass))
  barwidth <- RcssValue("perfbars", "barwidth", default=0.8)
  
  if (is.null(xlim)) {
    xlim <- c(0.5, length(labels)+0.5)
  }
  if (is.null(ylim)) {
    ylim <- c(0, max(d[[value.col]]))
  }
  
  parplot(xlim, ylim, xlim=xlim, ylim=ylim, type="n", axes=FALSE, frame=FALSE)
  
  for (i in seq_along(labels)) {
    i.vals <- as.numeric(d[d[[label.col]]==labels[i]][[value.col]])
    i.lab <- labels[i]
    i.mean <- mean(i.vals)
    i.errs <- quantile(i.vals, p=q)
    i.class <- ifelse(i.lab=="-", "zero", "")
    rect(i-barwidth/2, 0, i+barwidth/2, i.mean, Rcssclass=i.class)
    lines(rep(i, 2), i.errs, Rcssclass="errorbars")
  }
  
  axis(1, at=xlim, labels=c("", ""), line=0, Rcssclass="x")
  axis(1, at=seq_along(labels), labels=labels, lwd=0, Rcssclass="x")
  axis(2, at=ylim, labels=c("", ""), line=0, tck=0, Rcssclass="y")
  axis(2, labels=NA, line=0, Rcssclass="y")
  if (show.yvals) {
    axis(2, lwd=0, Rcssclass="y")
  }
  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=3, main, Rcssclass="main")
  mtext(side=3, submain, Rcssclass="submain")
}


#' summaries using box-and-whiskers, using two adjacent columns
#'
#' @param d data frame with columns for a categorical label and a value
#' @param value.cols character vector, columns in d containing values
#' @param label.cols character, column in d containing a label
#' (for horizontal axis)
#' @param q numeric of length 2, quantiles for errors bars
#' @param xlim numeric of length 2, limits for horizontal axis
#' @param ylim numeric of length 2, limits for vertical axis
#' @param axis.y numeric, spacing between x-axis and labels
#' @param xlab character, label for x-axis
#' @param main character, label for whole title
#' @param submain character, label just underneat title
#' @param Rcssclass character, style class
#' 
plot.perfbars2 <- function(d,
                          value.cols=c("precision", "precision_N"),
                          labels=c("best of 1", "best of N"),
                          q=c(0.25, 0.75),
                          xlim=NULL, ylim=NULL,
                          axis.y= -0.05,
                          xlab="", main="", submain="", 
                          Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("perfbars", Rcssclass))
  barwidth <- RcssValue("perfbars", "barwidth", default=0.8)
  
  if (is.null(xlim)) {
    xlim <- c(0.5, length(labels)+0.5)
  }
  d <- as.data.frame(d)
  if (is.null(ylim)) {
    ylim <- c(0, max(unlist(d[, value.cols])))
  }
  
  parplot(xlim, ylim, xlim=xlim, ylim=ylim, type="n", axes=FALSE, frame=FALSE)
  
  for (i in seq_along(labels)) {
    i.vals <- as.numeric(d[, value.cols[i]])
    i.mean <- mean(i.vals)
    i.errs <- quantile(i.vals, p=q)
    rect(i-barwidth/2, 0, i+barwidth/2, i.mean)
    lines(rep(i, 3), sort(c(i.errs, i.mean)), Rcssclass="errorbars")
  }
  
  axis(1, at=xlim, labels=c("", ""), line=0, Rcssclass="x")
  text(seq_along(labels), axis.y*ylim[2], labels, Rcssclass="x")
  axis(2, at=ylim, labels=c("", ""), line=0, tck=0, Rcssclass="y")
  axis(2, labels=NA, line=0, Rcssclass="y")
  axis(2, lwd=0, Rcssclass="y")
  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=3, main, Rcssclass="main")
  mtext(side=3, submain, Rcssclass="submain")
}



