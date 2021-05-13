# draw trends as boxes


#' sets of boxes
#'
#' @param data list of lists, final components should be numerical values
#' @param adjust numeric, passed to density()
#' @param n integer, passed to density()
#' @param cut numeric, passed to density()
#' @param xlim numeric of length 2, recommended to leave NULL
#' @param ylim numeric of length 2,
#' @param show.xaxis logical, toggle visibility of labels on x axis 
#' @param show.yaxis logical, toggle visibility of labels on y axis 
#' @param xlab character
#' @param ylab character
#' @param main character
#' @param Rcssclass character, style class
#' 
plot.groupviolins = function(data, adjust=1.5, n=128, cut=1, 
                             xlim=NULL, ylim=NULL, main=NULL,
                             show.xaxis=TRUE, show.yaxis=TRUE,
                             xlab="", ylab="",
                             Rcssclass=c()) {
  stop("deprecated")
  RcssCompulsoryClass = RcssGetCompulsoryClass(c("groupviolins", Rcssclass))
  space = RcssValue("groupviolins", "space", default=0.2)
  width = RcssValue("groupviolins", "width", default=08)
  
  n.groups = length(data)
  n.pergroup = length(data[[1]])
  names.groups = names(data)
  names.sub = names(data[[1]])

  if (is.null(xlim)) {
    xlim = c(-space/2, (n.groups-1)*space + (n.pergroup*n.groups)+ space/2)
  }
  if (is.null(ylim)) {
    ylim = range(unlist(data))
  }
  
  par()
  plot(xlim, ylim, xlim=xlim, ylim=ylim)
  
  if (show.xaxis) {
    axis(1, at=xlim, labels=c("", ""), line=0, tck=0)
    i.groups = seq_len(n.groups)
    group.centers = (i.groups-1)*n.pergroup + (i.groups-1)*space + n.pergroup/2
    axis(1, at=group.centers, labels=names(data), lwd=0, Rcssclass="x")
    mtext(side=1, xlab, Rcssclass="x")
  }
  if (show.yaxis) {
    axis(2, at=ylim, labels=c("", ""), line=0, tck=0)
    axis(2, labels=NA, line=0, Rcssclass="y")
    axis(2, lwd=0, Rcssclass="y")
    mtext(side=2, ylab, Rcssclass="y")
  }
  if (!is.null(main)) {
    mtext(side=3, main, Rcssclass="main")
  }
  
  x = 0.5
  for (i in seq_len(n.groups)) {
    for (j in seq_len(n.pergroup)) {
      ij.data = data[[i]][[j]]
      ij.median = median(ij.data)
      ij.density = density(ij.data, adjust=adjust, n=n, cut=cut)
      ij.density$y = (width/2)*ij.density$y / max(ij.density$y)
      ij.x = x+c(-rev(ij.density$y), ij.density$y)
      ij.y = c(rev(ij.density$x), ij.density$x)
      polygon(ij.x, ij.y, Rcssclass=names.sub[j])
      x = x + 1
    }
    x = x + space
  }  
}

