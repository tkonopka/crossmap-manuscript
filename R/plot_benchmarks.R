# plot functions for explaining benchmark sets


#' Draw a diagram with a gene pool, some subsets, and labels
#'
#' Genepool rect always appears in unit interval
#'
#' @param n.dots integer length 2, number of dots in the genepool grid
#' @param xlim numeric length 2, limits of plot.
#' @param ylim numeric length 2, limits of plot
#'
schematic.genepool <- function(n.dots=c(8,12), xlim=c(0,1), ylim=c(0, 1)) {

  # Rcss styling
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("genepool"))
  border.space <- RcssValue("genepool", "border.space", default=1)
  lab.space <- RcssValue("genepool", "lab.space", default=0.1)
  lab.offset <- RcssValue("genepool", "lab.offset", default=0.07)
  legend.w <- RcssValue("genepool", "legend.w", default=0.05)
  legend.offset <- RcssValue("genepool", "legend.offset", default=0.02)
  
  parplot(xlim, ylim, xlim=xlim, ylim=ylim, Rcssclass="empty")
  
  # draw dots and an outer boundary
  rect(0, 0, 1, 1)
  interval.x <- 1/(n.dots[1]+2)
  interval.y <- 1/(n.dots[2]+2)
  offset.x <- interval.x*border.space
  offset.y <- interval.y*border.space
  dots.x <- seq(offset.x, 1-offset.x, length=n.dots[1])
  dots.y <- seq(offset.y, 1-offset.y, length=n.dots[2])
  interval.x <- dots.x[3]-dots.x[2]
  interval.y <- dots.y[3]-dots.y[2]
  dots.xy <- expand.grid(list(x=dots.x, y=dots.y))
  points(dots.xy[,1], dots.xy[,2])

  # some example genesets
  A <- list(x=c(0.6, 3.4, 3.4, 0.6),
           y=c(0.6, 0.6, 3.4, 3.4))
  B <- list(x=c(3.6, 6.4, 6.4, 3.6),
           y=c(0.6, 0.6, 5.4, 5.4))  
  C <- list(x=c(1.6, 4.4, 4.4, 1.6),
           y=c(2.6, 2.6, 7.4, 7.4))
  D <- list(x=c(4.6, 6.4, 6.4, 3.6, 2.6, 2.6, 4.6),
           y=c(4.6, 4.6, 9.4, 9.4, 9.4, 7.6, 7.6))
  to.genepool <- function(z) {
    list(x=interval.x*(z$x-1)+offset.x,
         y=1-interval.y*(z$y-1)-offset.y)
  }
  roundedpolygon(to.genepool(A), p=0.1, Rcssclass="A")
  roundedpolygon(to.genepool(B), p=0.1, Rcssclass="B")
  roundedpolygon(to.genepool(C), p=0.1, Rcssclass="C")
  roundedpolygon(to.genepool(D), p=0.1, Rcssclass="D")
  
  # label on top of gene pool
  text(0, 1+lab.offset+lab.space, "Gene pool", Rcssclass="main")
  
  # legend labeling genes and sets  
  points((legend.w/2), -lab.offset-lab.space, Rcssclass="legend")
  text(legend.w + legend.offset, -lab.offset-lab.space, "gene",
       Rcssclass="legend")
  roundedpolygon(c(0, legend.w, legend.w, 0),
               -lab.offset - lab.space*c(1.5, 1.5, 2.5, 2.5),
               Rcssclass="legend")
  text(legend.w + legend.offset, -lab.offset-2*lab.space, "gene set",
       Rcssclass="legend")
}


#' draw a benchmark set
#'
#' @param position numeric of length 2, coordinates of top-left corner
#' @param width numeric, width of benchmark set
#' @param content character vector, style classes for
#' @param label.inner character, text to write inside the benchmark polygon
#' @param label.outer character, text to write beside the benchmark polygon
#' @param Rcssclass character, style class
#'
addBenchmarkSet <- function(position=c(1.3, 1), width=0.2, content=NULL,
                           label.inner="", label.outer="",
                           Rcssclass=c()) {
  # Rcss styling
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("benchmark", Rcssclass))
  height <- RcssValue("benchmark", "height", default=0.2)
  h2 <- height/2
  lab.offset <- RcssValue("benchmark", "lab.offset", default=0.1)
  
  x <- position[1]
  y <- position[2]
  if (identical(content, NULL)) { 
    text(x+width/2, y, label.inner, Rcssclass="inner")
  } else {
    n <- length(content)
    dots.x <- seq(x, x+width, length=2+n)[2:(n+1)]
    u <- unique(content)
    for (i in seq_along(u)) {
      i.which <- which(content==u[i])
      points(dots.x[i.which], rep(y, length(i.which)), Rcssclass=u[i])
    }
  }
  roundedpolygon(x=c(x, x, x+width, x+width), y=c(y+h2, y-h2, y-h2, y+h2))  
  text(x+width+lab.offset, y, label.outer, Rcssclass="outer")
}


#' a compound figure with a genepool and some further components
#'
schematic.genepool.benchmarks <- function(xlim=c(-0.1, 2.5)) {
  schematic.genepool(ylim=c(-0.45, 1.45), xlim=xlim,
                     n.dots=c(6,9))
  benchmark.x <- 1.35
  benchmark.y <- c(1.45, 0.8, 0.15)
  text(benchmark.x, benchmark.y[1]-0.04, "Components (comp)",
       Rcssclass=c("benchmark", "title"))
  addBenchmarkSet(c(benchmark.x, benchmark.y[1]-0.2), width=0.3,
                  content=c("A", "A", "A"), label.outer="comp=1")
  addBenchmarkSet(c(benchmark.x, benchmark.y[1]-0.4), width=0.6,
                  content=rep(c("B", "C", "D"), each=2),
                  label.inner = "50%", label.outer="comp=3")
  text(benchmark.x, benchmark.y[2]-0.04, "Coverage (cov)",
       Rcssclass=c("benchmark", "title"))
  addBenchmarkSet(c(benchmark.x, benchmark.y[2]-0.2), width=0.6,
                  content=rep("A", 9), label.outer="cov=1")
  addBenchmarkSet(c(benchmark.x, benchmark.y[2]-0.4), width=0.6,
                  content=rep("D", 4), label.outer="cov=0.25")
  text(benchmark.x, benchmark.y[3]-0.04, "Signal (sig)",
       Rcssclass=c("benchmark", "title"))
  addBenchmarkSet(c(benchmark.x, benchmark.y[3]-0.2), width=0.4,
                  content=rep("B", 4),
                  label.outer="sig=1")
  addBenchmarkSet(c(benchmark.x, benchmark.y[3]-0.4), width=0.6,
                  content=c("A", "A", "A", "O", "O", "O"),
                  label.outer="sig=0.5")
  Arrows(1.05, 0.7, benchmark.x-0.1, 0.9, col="#222222", lwd=2, arr.length=0.2)
  Arrows(1.05, 0.5, benchmark.x-0.1, 0.5, col="#222222", lwd=2, arr.length=0.2)
  Arrows(1.05, 0.3, benchmark.x-0.1, 0.1, col="#222222", lwd=2, arr.length=0.2)
}

