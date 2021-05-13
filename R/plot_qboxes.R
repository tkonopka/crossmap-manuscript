# plot functions for horizontal boxplots with quantiles



#' distribution summaries using box-and-whiskers
#'
#' @param d data frame with columns for a label, and columns with quantiles
#' @param label.col character, column in d containing a label for
#' horizontal axis
#' @param value.col character vector, columns in d containing values
#' @param q character vector with values denoting box quantiles
#' (Columns in d must contain columns labels as "VALUECOL_qQ")
#' @param xlim numeric of length 2, limits for horizontal axis
#' @param ylim numeric of length 2, limits for vertical axis
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param ylab character, label for y-axis
#'
plot.qboxes = function(d, label.col="", value.col="",
                       q=c("05", "25", "50", "75", "95"),
                       xlim=NULL, ylim=NULL,
                       xlab="", ylab="", main="",
                       Rcssclass=c()) {
  stop("deprecated")
  RcssCompulsoryClass = RcssGetCompulsoryClass(c("qboxes", Rcssclass))
  barwidth = RcssValue("qboxes", "barwidth", default=0.8)
  barheight = barwidth/length(value.col)
  
  # simplify the data for just this plot
  value.cols = paste0(rep(value.col, each=length(q)), "_q", q)
  data = as.matrix(as.data.frame(d)[, value.cols, drop=FALSE])
  rownames(data) = d[[label.col]]
  
  if (is.null(xlim)) {
    xlim = c(0, max(as.numeric(data)))
  }
  if (is.null(ylim)) {
    ylim = c(-nrow(data), 0)
  }

  parplot(xlim, ylim, xlim=xlim, ylim=ylim, type="n",
          axes=FALSE, frame=FALSE)

  # draw the box-and-whiskers
  y = -(1-barwidth)/2
  for (i in seq_len(nrow(data))) {
    for (j in seq_along(value.col)) {
      value.j = value.col[j]
      i.h = c(y, y-barheight)
      ij.q = data[i, paste0(value.col, "_q", q)]
      lines(c(ij.q[1:2], NA, ij.q[4:5]), rep(mean(i.h), 5),
            Rcssclass=c("whiskers", value.j))
      rect(ij.q[2], i.h[1], ij.q[4], i.h[2], Rcssclass=c("box", value.j))
      lines(rep(ij.q[3], 2), i.h, Rcssclass=c("mid", value.j))
      y = y - barheight
    }    
    y = y - (1-barwidth)
  }
  axis(2, at=rev(seq(-nrow(data), -1)+0.5), labels=rownames(data),
       Rcssclass="y")
  axis(3, at=xlim, labels=c("", ""), line=0, tck=0)
  axis(3, labels=NA, line=0, Rcssclass="x")
  axis(3, lwd=0, Rcssclass="x")
  mtext(side=3, xlab, Rcssclass="xlab")
  mtext(side=2, ylab, Rcssclass="ylab")
  mtext(side=3, main, Rcssclass="main")

}

