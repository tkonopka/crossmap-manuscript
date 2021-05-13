# plot function for Crossmap.Rmd



#' create a series of plots with plot.graph
#'
#' @param g matrix with graph layout (two columns)
#' @param edges data frame with true edge connections
#' @param nn.tab data frame with objects and nearest neighbors
#' @param nn.series integer vector, used to define the series
#' @param main character vector with titles for each panel
#'
plot.graph.series <- function(g, edges=NULL, nn.tab=NULL,
                             nn.series=c(0,1,2,3), main="",
                             show.labels=FALSE, panel.labels="", ...) {

  if (length(main) != length(nn.series)) {
    temp <- main
    main <- rep("", length(nn.series))
    main[1:length(temp)] = temp
  }
  for (i in seq_along(nn.series)) {
    nn <- nn.series[i]
    if (nn==0) {
      plot.graph(g, nn.tab=edges, main=main[i],
                 show.lines="simple", show.labels=show.labels,
                 Rcssclass="series", ...)
    } else {
      plot.graph(g, nn.tab=nn.tab[rank<=nn], main=main[i],
                 show.lines="rank", show.labels=FALSE, 
                 Rcssclass="series", ...)
    }
    multipanelLabel(panel.labels[i])
  }
}



#' create a grid of graphs with
#'
#' @param g matrix with graph layout
#' @param edges data frame with true edge connections
#' @param nn data table with $id and $target
#' @param suffixes named character vector, names are used to look up
#' components in nn
#' @param nn.rank integer, show connections
#'
plot.graph.grid <- function(g, edges=NULL, nn=NULL,
                           suffixes=c(term="", parents="with parents",
                                      siblings="with siblings",
                                      ancestors="with ancestors"),
                           nn.rank=4) {
  plot.label <- function(label="", sublabel="", Rcssclass=c()) {
    par()
    plot(c(0,1), c(0, 1), type="n")
    text(0.5, 0.5, paste0(label, "\n", sublabel),
         Rcssclass=c("graph", "gridlabel", Rcssclass))
  }
  # top-left corner is empty
  par(mai=rep(0.001, 4))
  plot(0,0, type="n", Rcssclass="empty")
  # first row contains only labels
  for (.type in names(suffixes)) {
    plot.label("data", suffixes[.type], Rcssclass="top")
  }
  for (.config.type in names(suffixes)) {
    plot.label("config", suffixes[.config.type], Rcssclass="side")
    for (.data.type in names(suffixes)) {
      .type <- paste0("config", .config.type, ".data", .data.type)
      .nn.tab <- nn[grepl(.data.type, data.label) &
                     grepl(.config.type, config.label)]
      plot.graph(g, .nn.tab[rank<=nn.rank], show.lines="rank",
                 show.labels=FALSE,
                 Rcssclass="series")
    }
  }  
}


#' create a plot with a network (graph)
#'
#' @param g matrix with two columns for x-y coordinates
#' @param nn.tab data frame/table with objects and nearest neighbors
#'
plot.graph <- function(g, nn.tab=NULL, main="",
                      show.lines=c("simple", "none", "rank"),
                      show.labels=TRUE,
                      legend.pos=NA, legend.main="Predictions",
                      Rcssclass=c()) {
  
  show.lines <- match.arg(show.lines)
  xlim <- range(g[,1])
  ylim <- range(g[,2])
  xwidth <- xlim[2]-xlim[1]
  ywidth <- ylim[2]-ylim[1]
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("graph", Rcssclass))
  xu <- RcssValue("graph", "x", default=0.01)*xwidth
  yu <- RcssValue("graph", "y", default=0.02)*ywidth
  
  par()
  plot(xlim, ylim, xlim=xlim, ylim=ylim, type="n")
  
  # draw the lines
  nn.tab <- as.matrix(nn.tab)
  if (show.lines=="simple") {
    nn.list <- list(rank_0=nn.tab)
  } else if (show.lines == "rank") {
    nn.list <- split(data.table(nn.tab[, c("id", "target")]), nn.tab[, "rank"])
    names(nn.list) <- paste0("rank_", names(nn.list))
  }
  line.col <- c()
  line.lwd <- c()
  nn.names <- names(nn.list)
  if (show.lines %in% c("simple", "rank")) {
    for (r in rev(nn.names)) {
      nn.tab <- as.matrix(nn.list[[r]])
      nn.3 <- nrow(nn.tab)*3
      xcoord <- rep(NA, nn.3)
      ycoord <- rep(NA, nn.3)
      xcoord[seq(1, nn.3, by=3)] <- g[nn.tab[,1], 1]
      ycoord[seq(1, nn.3, by=3)] <- g[nn.tab[,1], 2]
      xcoord[seq(2, nn.3, by=3)] <- g[nn.tab[,2], 1]
      ycoord[seq(2, nn.3, by=3)] <- g[nn.tab[,2], 2]
      lines(xcoord, ycoord, Rcssclass=r)
      line.col[r] <- RcssValue("lines", "col", Rcssclass=r)
      line.lwd[r] <- RcssValue("lines", "lwd", Rcssclass=r)
    }
  }
  
  # label the items
  if (show.labels) {
    rect(g[,1]-nchar(rownames(g))*xu, g[,2]-yu,
         g[,1]+nchar(rownames(g))*xu, g[,2]+yu,
         col="#ffffff", Rcssclass="label")
    text(g[,1], g[,2], rownames(g), Rcssclass="label")
  }

  mtext(side=3, main, Rcssclass="main")

  if (!is.na(legend.pos)) {
    legend(legend.pos, gsub("_", " ", nn.names),
           title=legend.main,
           col=line.col[nn.names], lwd=line.lwd[nn.names])
  }
  
}

