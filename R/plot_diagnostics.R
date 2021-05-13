# plot functions used during manual diagnostics steps


#' draw weights
#'
#' @param filepath path to table of weights
#' @param sort.by character, column in data frame to sort by
#' @param decreasing logical, direction of sort
#' @param num.items integer, number or items to display in plot
#' @param ids character vector, columns to show in diagram
#' @param Rcssclass character, style class
#'
plot.diagnostic.weights <- function(filepath,
                                   sort.by=NULL,
                                   decreasing=TRUE,
                                   num.items=60,
                                   ids=NULL,
                                   Rcssclass=c()) {
  d <- fread(filepath)
  if (!is.null(sort.by)) {
    d <- d[order(d[[sort.by]], decreasing=decreasing)]
  }
  if (!is.null(ids)) {
    setcolorder(d, ids)
  }
  ids <- setdiff(colnames(d), c("_feature", "_min", "_max"))

  dplot <- d
  if (nrow(d)>num.items) {
    dplot <- d[seq_len(num.items)]
  }

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("diagnostics", Rcssclass))
  cols <- RcssValue("diagnostics", "col", default=c(1,2,3,4,5))
  par(mfrow=c(1, length(ids)))
  for (i in seq_along(ids)) {
    values <- rev(setNames(dplot[[ids[i]]], dplot[["_feature"]]))
    barplot(values, horiz=TRUE, col=cols[i])
    mtext(side=1, "weight", Rcssclass="xlab")
    mtext(side=3, ids[i], Rcssclass="main")
  }
  invisible(d)
}

