# draw 2d plots based on groups summarized into mean+IQR


#' trends as lines with intervals
#'
#' @param data data table with columns $series and columns specified in xy.
#' Items in $y should be lists with values, which will be summarized into a
#' point and interval
#' @param xy character of length 2, column in data to use on x axis and y axis
#' @param q numeric of length 2, quantiles to determine quantiles
#' @param xlim numeric of length 2
#' @param ylim numeric of length 2
#' @param show.xaxis logical, toggle visibility of labels on x axis 
#' @param show.yaxis logical, toggle visibility of labels on y axis 
#' @param xlab character
#' @param ylab character
#' @param main character
#' @param Rcssclass character, style class
#'
#' @return
plot.trends <- function(data, xy=colnames(data)[1:2],
                        q=c(0.25, 0.5),
                        xlim=NULL, ylim=NULL,
                        show.xaxis=TRUE, show.yaxis=TRUE,
                        xlab="", ylab="", main="",
                        Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("trends", Rcssclass))
  
  if (is.null(xlim)) {
    xlim <- range(data[[xy[1]]])
  }
  if (is.null(ylim)) {
    ylim <- range(unlist(data[[xy[2]]]))
  }
  
  par()
  plot(xlim, ylim, type="n", xlab="", ylab="")

  axis(1, at=xlim, labels=c("", ""), line=0, tck=0, Rcssclass="x")
  axis(1, labels=NA, line=0, Rcssclass="x")
  if (show.xaxis) {
    axis(1, lwd=0, Rcssclass="x")
    mtext(side=1, xlab, Rcssclass="x")
  }
  axis(2, at=ylim, labels=c("", ""), line=0, tck=0, Rcssclass="y")
  axis(2, labels=NA, line=0, Rcssclass="y")
  if (show.yaxis) {
    axis(2, lwd=0, Rcssclass="y")
    mtext(side=2, ylab, Rcssclass="y")
  }
  if (!is.null(main)) {
    mtext(side=3, main, Rcssclass="main")
  }

  dataseries <- split(data, data$series)
  for (s in names(dataseries)) {
    sdata <- dataseries[[s]]
    x <- sdata[[xy[1]]]
    y <- sapply(sdata[[xy[2]]], mean)
    ylo <- sapply(sdata[[xy[2]]], quantile, q[1])
    yhi <- sapply(sdata[[xy[2]]], quantile, q[2])
    yint <- rbind(ylo, yhi, NA)
    points(x, y, Rcssclass="s")
    lines(rep(x, each=3), as.numeric(rbind(ylo, yhi, NA)), Rcssclass=s)
    lines(x, y, Rcssclass=s)
  }

}


#' trends as boxes. These are boxplots arranged in groups.
#'
#' @param data data table with $series, and columns specifed by xy
#' @param xy character of length 2, column in data to use on x axis and y axis
#' xy[1] is a label for groups on the horizontal axis
#' xy[2] is column containing values for y-axis
#' @param x.values values for x-axis, in order of appearance. 
#' @param series.column character, column in data that identifies components
#' in series
#' @param series.values content in series.column, in order of appearance
#' @param q numeric of length 5, quantiles for box features
#' @param xlim numeric of length 2, recommended to leave NULL
#' @param ylim numeric of length 2,
#' @param show.xaxis logical, toggle visibility of labels on x axis 
#' @param show.yaxis logical, toggle visibility of labels on y axis 
#' @param at list, position of tick marks
#' @param xlab character
#' @param ylab character
#' @param main character
#' @param Rcssclass character, style class
#' 
plot.boxtrends <- function(data, xy=colnames(data)[1:2], x.values=NULL,
                           series.column=NULL,
                           series.values=NULL,
                           q=c(0.05, 0.25, 0.5, 0.75, 0.95),
                           xlim=NULL, ylim=NULL, main=NULL,
                           show.xaxis=TRUE, show.yaxis=TRUE,
                           show.box=FALSE,
                           at=list(y=seq(0, 5)),
                           xlab="", ylab="",
                           Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(
    c("boxtrends", series.column, Rcssclass))
  space <- RcssValue("boxtrends", "space", default=0.2)
  width <- RcssValue("boxtrends", "width", default=0.8)
  w2 <- width/2

  if (is.null(x.values)) {
    x.values <- sort(unique(data[[xy[1]]]))
  }
  if (is.null(series.values) & !is.null(series.column)) {
    series.values <- sort(unique(data[[series.column]]))
  }
  series.length <- length(series.values)
  num.x <- length(x.values)
  
  if (is.null(xlim)) {
    xlim <- c(-space/2, (num.x-1)*space + (series.length*num.x)+ space/2)
  }
  if (is.null(ylim)) {
    ylim <- range(unlist(data[[xy[2]]]))
  }
  
  par()
  plot(xlim, ylim, xlim=xlim, ylim=ylim)
  if (show.box) {
    box()
  }
  
  axis(1, at=xlim, labels=c("", ""), line=0, tck=0)
  if (show.xaxis) {
    i.groups <- seq_len(num.x)
    sl <- series.length
    group.centers <- (i.groups-1)*sl + (i.groups-1)*space + sl/2
    axis(1, at=group.centers,
         labels=gsub("fisher", "Fisher", x.values),
         lwd=0, Rcssclass="x")
    mtext(side=1, xlab, Rcssclass="x")
  }
  axis(2, at=ylim, labels=c("", ""), line=0, tck=0)
  at.y <- at$y
  at.y <- at.y[at.y >= ylim[1] & at.y <= ylim[2]]
  axis(2, at=at.y, labels=NA, line=0, Rcssclass="y")
  if (show.yaxis) {
    axis(2, lwd=0, at=at.y, Rcssclass="y")
    mtext(side=2, ylab, Rcssclass="y")
  }
  if (!is.null(main)) {
    mtext(side=3, main, Rcssclass="main")
  }
  
  x <- 0.5
  j.colors <- rep(NA, series.length)
  for (j in seq_len(series.length)) {
    j.target <- series.values[j]
    j.class <- paste(gsub("\\.", "_", c(series.column, j.target)), collapse="")
    j.colors[j] <- RcssValue("rect", "col", default="#000000",
                             Rcssclass=j.class)
  }

  values <- data[[xy[2]]]
  for (i in seq_len(num.x)) {
    i.value <- x.values[i]
    for (j in seq_len(series.length)) {
      j.target <- series.values[j]
      j.col <- j.colors[j]
      ij.which <- which(data[[xy[1]]]==i.value &
                          data[[series.column]]==j.target)
      ij.data <- values[ij.which]
      if (length(ij.data)>0) {
        ij.q <- quantile(ij.data, p=q)
        lines(rep(x, 5), c(ij.q[1:2], NA, ij.q[4:5]), col=j.col,
              Rcssclass=c("whiskers"))
        rect(x-w2, ij.q[2], x+w2, ij.q[4], col=j.col, Rcssclass="box")
        lines(c(x-w2, x+w2), rep(ij.q[3], 2), Rcssclass="mid")
      }
      x <- x + 1
    }
    x <- x + space
  }

}



plot.boxtrends.legend <- function(legend.pos, legend.main="",
                                  legend.values=c()) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("boxtrends", "legend"))
  par()
  plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), type="n", axes=FALSE, frame=FALSE)
  
  j.colors <- rep(NA, length(legend.values))
  for (j in seq_along(legend.values)) {
    j.target <- gsub("\\.", "_", legend.values[j])
    j.colors[j] <- RcssValue("rect", "col",
                             Rcssclass=paste0(tolower(legend.main), j.target))
  }
  
  leg.w <- RcssValue("boxtrends", "leg.w", default=0.8)
  leg.h <- RcssValue("boxtrends", "leg.h", default=0.02)
  leg.info <- legend(legend.pos[1], legend.pos[2],
                     c(legend.main, legend.values),
                     plot=FALSE)
  text(leg.info$text$x[1], leg.info$text$y[1], legend.main,
       Rcssclass="legend")
  rect(leg.info$text$x[-1], leg.info$text$y[-1]-leg.h,
       leg.info$text$x[-1]+leg.w, leg.info$text$y[-1]+leg.h, col=j.colors,
       Rcssclass="legend")
  text(leg.info$text$x[-1]+leg.w*1.5, leg.info$text$y[-1], legend.values,
       Rcssclass="legend") 
}

