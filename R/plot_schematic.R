# plot functions for schematic vector matching and decomposition



#' add an Arrow to an existing plot
#'
#' @param point0 numeric of length 2, (x0, y0)
#' @param point1 numeric of length 2, c(x1, y1)
addArrow <- function(point0, point1, Rcssclass="target", ...) {
  if (!"schematic" %in% RcssGetCompulsoryClass()) {
    RcssCompulsoryClass <- RcssGetCompulsoryClass("schematic")
  }
  col <- RcssValue("Arrow", "col", default="#000000", Rcssclass=Rcssclass)
  lwd <- RcssValue("Arrow", "lwd", default=1, Rcssclass=Rcssclass)
  len <- RcssValue("Arrow", "arr.length", default=0.2, Rcssclass=Rcssclass)
  adj <- RcssValue("Arrow", "arr.adj", default=1, Rcssclass=Rcssclass)
  Arrows(point0[1], point0[2], point1[1], point1[2], xpd=1,
         col=col, lwd=lwd, arr.length=len, arr.adj=adj, ...)
}



#' draw a diagram explaining matching of a query to nearest neighbors
#'
#' @param query matrix with coordinates (cannot be null)
#' @param targets matrix with coordinates (cannot be null)
#' @param documents matrix with coordinates
#' @param show.target.points vector of target indexes to draw with a
#' highlighted marker
#' @param show.target.links vector of target indexes to show with links,
#' @param show.doc.links vector of document indexes to show with links,
#' @param show.query.vector logical, arrow from origin to the query
#' @param show.target.vectors vector of target indexes to show with vectors,
#' @param xlim numeric length 2
#' @param ylim numeric length 2
#' @param Rcssclass character, style class
#'
schematic.neighbors <- function(query=NULL, targets=NULL, documents=NULL,
                                show.target.points=NULL,
                                show.target.links=NULL,
                                show.doc.links=c(),
                                show.query.vector=TRUE,
                                show.target.vectors=show.target.links,
                                xlim=c(-0.6, 5.0), ylim=NULL,
                                show.axis.labels=TRUE,
                                Rcssclass=c()) {
  if (is.null(ylim)) {
    ylim <- xlim
  }
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", Rcssclass))
  # collect some custom graphics style from Rcss
  vec.axis.col <- RcssValue("Arrow", "col", default="#222222",
                            Rcssclass="axis")
  vec.axis.length <- RcssValue("Arrow", "arr.length", default=0.2,
                               Rcssclass="axis")
  vec.target.lwd <- RcssValue("Arrow", "lwd", default=1.5,
                              Rcssclass="target")
  vec.target.length <- RcssValue("Arrow", "arr.length", default=0.3,
                                 Rcssclass="target")
  
  par()
  plot(xlim, ylim, xlim=xlim, ylim=ylim, Rcssclass="empty")

  # draw axes
  Arrows(xlim[1], 0, xlim[2], 0, xpd=1,
         col=vec.axis.col, arr.length=vec.axis.length, arr.type="triangle")
  Arrows(0, ylim[1], 0, ylim[2], xpd=1,
         col=vec.axis.col, arr.length=vec.axis.length, arr.type="triangle")
  if (show.axis.labels) {
    text(xlim[2], 0, "x", Rcssclass=c("q2", "x"))
    text(0, ylim[2], "y", Rcssclass=c("q4", "y"))
  }

  # position of the query point
  qx <- query[1,1]
  qy <- query[1,2]
  
  # draw the connectors from the query to the target
  for (i.target in show.target.links) {
    lines(c(qx, targets[i.target,1]),
          c(qy, targets[i.target,2]), Rcssclass="q_t")
  }
  
  if (!is.null(documents) & length(show.doc.links)>0) {
    for (i.doc in show.doc.links) {
      lines(c(qx, documents[i.doc, 1]),
            c(qy, documents[i.doc, 2]), Rcssclass="q_d")
      for (i.target in show.target.links) {
        lines(c(documents[i.doc, 1], targets[i.target, 1]),
              c(documents[i.doc, 2], targets[i.target, 2]),
              Rcssclass=c("d_t", rownames(targets)[i.target]))
      }
    }
  }

  # draw arrows to the the points
  if (length(show.target.vectors)) {
    Arrows(0, 0,
           targets[show.target.vectors, 1], targets[show.target.vectors, 2],
           xpd=1,
           arr.adj=1, arr.length=vec.target.length, lwd=vec.target.lwd)
  }
  if (show.query.vector) {
    Arrows(0, 0, query[, 1], query[, 2], xpd=1,
           arr.adj=1, arr.length=vec.target.length, lwd=vec.target.lwd)
  }
  
  if (!identical(show.target.points, NULL)) {
    temp <- seq_len(nrow(targets)) %in% show.target.points
    targets.plain <- targets[!temp, , drop=FALSE]
    targets.highlight <- targets[temp, , drop=FALSE]
    points(targets.plain[,1], targets.plain[,2], Rcssclass="target")
    points(targets.highlight[,1], targets.highlight[,2],
           Rcssclass=c("target", "highlight"))
  } else {
    points(targets[,1], targets[,2], Rcssclass="target")
  }
  points(documents[,1], documents[,2], Rcssclass="doc")
  points(query[,1], query[,2], Rcssclass="query")

}



#' draw a set of elements in a new  plot area (legend for nearest neighbors)
schematic.legend.nn <- function(xlim=c(0, 1), ylim=c(0, 1),
                                vec.x=0.3, legend.x=0.45) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic"))
  par(Rcssclass="legend")
  plot(xlim, ylim, xlim=xlim, ylim=ylim, Rcssclass="empty")

  # legend for axis vectors
  addArrow(c(0, 1), c(vec.x, 1), Rcssclass="axis", arr.type="triangle")
  text(vec.x, 1, "x", Rcssclass=c("q2", "x"))
  text(legend.x, 1, "axis", Rcssclass="legend")

  # legend for misc lines
  l.y <- 0.8
  lines(c(0, vec.x), rep(l.y+0.04, 2), Rcssclass="q_t")
  lines(c(0, vec.x), rep(l.y, 2), Rcssclass="q_d")
  lines(c(0, vec.x), rep(l.y-0.04, 2), Rcssclass="d_t")
  text(legend.x, l.y, "connections", Rcssclass="legend")
  
  # legend for query object
  q.y <- 0.6
  points(vec.x/2, q.y, Rcssclass="query")
  text(vec.x/2, q.y, "q", Rcssclass=c("q2"))
  text(legend.x, q.y, "query", Rcssclass="legend")
  
  # legend for a target object
  t.y <- 0.4
  points(vec.x/2, t.y, Rcssclass="target")
  text(vec.x/2, t.y, "A", Rcssclass=c("q2"))
  text(legend.x, t.y, "target", Rcssclass="legend")

  # legend for document points
  d.y <- 0.15
  points(vec.x/2, d.y, Rcssclass="doc")
  text(legend.x, d.y, "auxiliary\ndocument", Rcssclass="legend")

}

#' draw a set of elements in a new plot area (legend for decomposition)
schematic.legend.decomposition <- function(xlim=c(0, 1), ylim=c(-0.05, 0.95),
                                           vec.x=0.3, legend.x=0.45) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic"))
  par(Rcssclass="legend")
  plot(xlim, ylim, xlim=xlim, ylim=ylim, Rcssclass="empty")

  # legend for axis vectors
  addArrow(c(0, 1), c(vec.x, 1), arr.type="triangle",
           Rcssclass=c("schematic", "axis"))
  text(vec.x, 1, "x", Rcssclass=c("q2", "x"))
  text(legend.x, 1, "axis", Rcssclass="legend")

  # legend for target vectors
  addArrow(c(0, 0.8), c(vec.x, 0.8), Rcssclass="target")
  text(legend.x, 0.8, "vector", Rcssclass="legend")

  # legend for misc lines
  l.y <- 0.6
  lines(c(0, vec.x), rep(l.y, 2), Rcssclass="q_t")
  text(legend.x, l.y, "connection", Rcssclass="legend")

  # two arrows for decompositions
  d.y <- 0.4
  addArrow(c(0, d.y), c(vec.x, d.y), Rcssclass=c("component"))
  text(legend.x, d.y, "component", Rcssclass="legend")

  d.y <- 0.2
  addArrow(c(0, d.y), c(vec.x, d.y), Rcssclass=c("residual"))
  text(legend.x, d.y, "residual", Rcssclass="legend")
  
  # legend for query object
  q.y <- 0.0
  points(vec.x/5, q.y, Rcssclass="query")
  text(vec.x/5, q.y, "q", Rcssclass=c("q2"))
  points(vec.x-vec.x/5, q.y, Rcssclass="target")
  text(vec.x-vec.x/5, q.y, "A", Rcssclass=c("q2"))  
  text(legend.x, q.y, "query, target", Rcssclass="legend")
}


#' draw a set of elements to complement an explanation of diffusion
schematic.legend.diffusion <- function(xlim=c(0, 1), ylim=c(0, 1),
                                       vec.x=0.3, legend.x=0.45) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic"))
  par(Rcssclass="legend")
  plot(xlim, ylim, xlim=xlim, ylim=ylim, Rcssclass="empty")
  
  # legend for axis vectors
  addArrow(c(0, 1), c(vec.x, 1), Rcssclass="axis", arr.type="triangle")
  text(vec.x, 1, "x", Rcssclass=c("q2", "x"))
  text(legend.x, 1, "axis", Rcssclass="legend")

  # legend for query object
  q.y <- 0.8
  points(vec.x/2, q.y, Rcssclass="query")
  text(vec.x/2, q.y, "q", Rcssclass=c("q2"))
  text(legend.x, q.y, "query", Rcssclass="legend")

  # legend for query object
  q.y <- 0.6
  points(vec.x/2, q.y, Rcssclass="query")
  text(vec.x/2, q.y, "q'", Rcssclass=c("q2"))
  text(legend.x, q.y, "diffused query", Rcssclass="legend")
  
  # legend for a target object
  t.y <- 0.4
  points(vec.x/2, t.y, Rcssclass="target")
  text(vec.x/2, t.y, "A", Rcssclass=c("q2"))
  text(legend.x, t.y, "target", Rcssclass="legend")

}


#' create a schematic of the diffusion algorithm
#'
#' @param docs list with sparse vectors
#' @param query numeric vector with query
#' @param diffused numeric vector
#' @param noise numeric,
#' @param Rcssclass character, style
#'
schematic.diffusion <- function(docs=list(), query, diffused,
                                noise=0.1,
                                Rcssclass=c()) {

  n.features <- max(unlist(docs))
  n.docs <- length(docs)

  RcssCompulsoryClass <- RcssGetCompulsoryClass(
    c("schematic", "diffusion", Rcssclass))
  lab.space <- RcssValue("diffusion", "lab.space", default=0.1) * n.docs
  lab.offset <- RcssValue("diffusion", "lab.offset", default=0.07) * n.features
  space <- RcssValue("diffusion", "space", default=0.1) * n.docs
  query.height <- RcssValue("diffusion", "query.height", default=0.05) * n.docs

  xlim <- c(0, n.features)
  ylim <- c(-n.docs-(2*query.height)-(2*lab.space), 0)
  par()
  plot(xlim, ylim)
  
  # draw the documents
  text(n.features/2, lab.space, "Features", Rcssclass=c("label", "x"))
  text(-lab.offset, -n.docs/2, "documents", Rcssclass=c("label", "y"))
  for (i in seq_along(docs)) {
    i.data <- c(docs[[i]], sample(1:n.features, n.features*noise))
    i.len <- length(i.data)
    rect(i.data-1, rep(-i, i.len), i.data, rep(-i+1, i.len), Rcssclass="cell")
  }
  rect(0,0, n.features, -n.docs, Rcssclass="outer")
  
  # draw query
  text(-lab.space, -n.docs-space-(0.5*query.height), "q ",
       Rcssclass=c("label", "query"))
  text(-lab.space, -n.docs-(2*space)-(1.5*query.height), "q'",
       Rcssclass=c("label", "query"))

  query <- pmax(0, pmin(1, query))
  diffused <- pmax(0, pmin(1, diffused))
  
  palette <- cividis(100, begin=0, end=1)
  rect(seq_along(query)-1, rep(-n.docs-space, length(query)),
       seq_along(query), rep(-n.docs-space-query.height, length(query)),
       col=palette[pmax(1, round(query*100))], Rcssclass="cell")
  rect(0, -n.docs-space, n.features, -n.docs-space-query.height,
       Rcssclass="outer")
  rect(seq_along(diffused)-1,
       rep(-n.docs-(2*space)-query.height, length(diffused)),
       seq_along(diffused),
       rep(-n.docs-(2*space)-(2*query.height), length(diffused)),
       col=palette[pmax(1, round(diffused*100))], Rcssclass="cell")
  rect(0, -n.docs-(2*space)-query.height,
       n.features, -n.docs-(2*space)-(2*query.height),
       Rcssclass="outer")

}

