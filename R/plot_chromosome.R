# scatter plot with values along a chromosome
# (this is like a scatter plot, but with chromosome-specific decorations


#' draw one chromosome and values
#'
#' @param d data frame
#' @param xy character, columns in d for axes
#' @param xlim numeric of length 2, limits for x-axis
#' @param ylim numeric of length 2, limits for y-axis
#' @param main character, label for whole plot
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param at list with components x and y, position of tick marks
#' @param at.labels list with components x and y, text labels to match argument at
#' @param genes.up character, set of up-regulated genes
#' @param genes.down character, set of down-regulated genes
#' @param legend.pos numeric of length 2 position of legend
#' @param legend.text character, text for the legend (two items)
#' @param Rcssclass character, style class
plot.chromosome <- function(d, xy=c("position", "log10.fc"),
                            xlim=NULL, ylim=NULL,
                            main="", xlab="position (Mbp)", ylab="fold-change",
                            at=list(x=seq(0, 2.5e8, by=2e7),
                                    y=log10(2^c(-3,-2,-1,0,1,2,3))),
                            genes.up=NULL,
                            genes.down=NULL,
                            legend.pos=NA, legend.text=c("gene", "DE gene"),
                            Rcssclass=c()) {
  
  d <- as.data.frame(d)
  if (!"gene_symbol" %in% colnames(d)) {
    stop("chromosome data must contain column 'gene_symbol'")
  }
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("chromosome", Rcssclass))
  
  # limits for the plot
  x <- xy[1]
  if (is.null(xlim)) {
    xlim <- c(0, max(d[,x]))
  }
  y <- xy[2]
  if (is.null(ylim)) {
    ylim <- c(min(d[,y]), max(d[,y]))
  }
  
  # start empty plot  
  parplot(xlim, ylim, xlim=xlim, ylim=ylim)
  lines(xlim, rep(0, 2), Rcssclass="centerline")
  
  all.genes <- unique(d$gene_symbol)
  genes.other <- setdiff(all.genes, c(genes.up, genes.down))
  if (length(genes.other)>0) {
    d.other <- d[d$gene_symbol %in% genes.other,, drop=FALSE]
    points(d.other[, x], d.other[, y])
  }
  if (length(genes.up)>0) {
    d.up <- d[d$gene_symbol %in% genes.up, ,drop=FALSE]
    points(d.up[, x], d.up[, y], Rcssclass=c("DE", "up"))
  }
  if (length(genes.down)>0) {
    d.down <- d[d$gene_symbol %in% genes.down, , drop=FALSE]
    points(d.down[, x], d.down[, y], Rcssclass=c("DE", "down"))
  }
  
  axis(1, at=xlim, label=c("", ""), line=0, tck=0, Rcssclass="x")
  axis(1, at=at$x, label=NA, line=0, Rcssclass="x")
  axis(1, at=at$x, label=paste0(at$x/1e6), lwd=0, Rcssclass="x")
  axis(2, at=ylim, label=c("", ""), line=0, tck=0, Rcssclass="y")
  axis(2, at=at$y, label=NA, line=0, Rcssclass="y")
  axis(2, at=at$y, label=10^at$y, lwd=0, Rcssclass="y")
  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=2, ylab, Rcssclass="ylab")
  mtext(side=3, main, Rcssclass="main")

  if (!identical(legend.pos, NA)) {
    col.other <- RcssValue("points", "col", default="#000000")
    col.up <- RcssValue("points", "col", default="#000000", Rcssclass="DE")
    legend(legend.pos[1], legend.pos[2], legend.text,
           col=c(col.other, col.up))
  }
}

