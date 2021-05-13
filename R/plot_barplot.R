# plot functions with bars

shorten <- function(x, maxlen) {
  toolong <- nchar(x) > maxlen & !is.na(x)
  result <- x
  if (sum(toolong, na.rm=TRUE)>0) {
    result[toolong] <- paste0(substr(x[toolong], 1, maxlen-3), "...")
  }
  result
}

#' draw a bar chart
#'
#' @param d data frame
#' @param value.col character, column in d with values
#' @param label.col character, column in d with labels for each bar
#' @param id.col character,
#' @param label.maxlen integer, maximum number of characters in a y-axis label
#' @param xlab character, label for x-axis
#' @param main character, label for entire chart
#' @param clean.labels logical, some ad-hoc shortening of labels
#' @param col character, color for bars
#' @param Rcssclass character, style class
#'
#' @param value
plot.outputs.barplot <- function(d, value.col, label.col=NULL, id.col=NULL,
                                 label.maxlen = 38, xlim=NULL,
                                 xlab="", main="", clean.labels=TRUE, col=NULL,
                                 Rcssclass=c()) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("outputs", Rcssclass))
  d <- as.data.frame(d)
  values <- d[[value.col]]
  labels <- as.character(d[[label.col]])

  if (clean.labels) {
    labels <- gsub(", score > 600", "", labels)
    labels <- gsub("expression in top", "top", labels)
    labels <- gsub("GTEX, ", "", labels)
    labels <- gsub("GENCODE genes, ", "", labels)
    labels <- gsub("Regulation of", "Reg. of", labels)
    labels <- shorten(labels, label.maxlen)
  }

  barwidth <- RcssValue("outputs", "barwidth", default=0.8)
  bw2 <- barwidth/2
  label.offset <- RcssValue("outputs", "label.offset", default=0.05)
  if (is.null(xlim)) {
    xlim <- c(0, max(as.numeric(values), na.rm=TRUE))
  }
  ylim <- c(-length(values), -1) + c(-barwidth, barwidth);

  parplot(xlim, ylim, xlim=xlim, ylim=ylim,
          type="n", axes=FALSE, frame=FALSE)
  y <- -1
  for (i in seq_len(length(values))) {
    if (!is.null(col)) {
      rect(0, y-bw2, values[i], y+bw2, col=col)
    } else {
      rect(0, y-bw2, values[i], y+bw2)
    }
    y <- y - 1
  }

  axis(3, at=xlim, labels=c("", ""), line=0, tck=0)
  axis(3, labels=NA, line=0, Rcssclass="x")
  axis(3, lwd=0, Rcssclass="x")
  mtext(side=3, xlab, Rcssclass="xlab")
  mtext(side=3, main, Rcssclass="main")
  if (!is.null(id.col)) {
    sources <- as.character(d[[id.col]])
    axis(2, at=-seq_len(length(labels))+label.offset, labels=sources,
         lwd=0, Rcssclass=c("y", "top"))
  } else {
    label.offset <- 0
  }
  axis(2, at=-seq_len(length(labels))-label.offset,
       labels=gsub("_", " ", labels),
       lwd=0, Rcssclass=c("y", "bottom"))
}



#' draw a bar chart with evaluation 
#'
#' @param x data frame, must contain columns label and value.col
#' @param xlab character, label for x axis
#' @param value.col character, column in data frame
#' @param label.col character, column in data frame to use for labeling groups
#' of columns
plot.evaluation.barplot <- function(x, xlab=value.col,
                                    value.col="precision",
                                    label.col="config.label") {
  stop("is this used?")
  RcssCompulsoryClass <- RcssGetCompulsoryClass("evaluation")
  data <- as.matrix(as.data.frame(x)[, value.col, drop=FALSE])
  rownames(data) <- x[[label.col]]
  xlim <- c(0, max(as.numeric(data)))
  lab.offset <- RcssValue("evaluation", "lab.offset", default=0.02)*(xlim[2])
  barwidth <- RcssValue("evaluation", "barwidth", default=0.8)
  par()
  plot(xlim, c(-nrow(x), 0), type="n", axes=FALSE, frame=FALSE)
  y <- -(1-barwidth)
  barheight <- barwidth/length(value.col)
  for (i in seq_len(nrow(data))) {
    for (j in seq_along(value.col)) {
      boxh <- c(y, y-barheight)
      boxw <- data[i, value.col[j]]
      rect(0, boxh[1], boxw, boxh[2], Rcssclass=value.col[j])
      y <- y - barheight
    }    
    y <- y - (1-barwidth)
  }
  axis(3, at=xlim, labels=c("", ""), line=0, tck=0)
  axis(3, labels=NA, line=0, Rcssclass="x")
  axis(3, lwd=0, Rcssclass="x")
  mtext(side=3, xlab, Rcssclass="xlab")
  text(-lab.offset, 0.5-seq_len(nrow(x)), rownames(data), Rcssclass="y")
}


#' draw a bar chart with diffusion hits
#'
#' @param d data table with many hits. Must have a unique value in "input" and
#' several "features"
#' @param n integer, number of rows to display
#' @param value.cols character, columns to shows as bars
#' @param remove.self logical, remove feature that overlap with the input query
#' @param xlim numerical of length 2
#' @param legend.pos numeric of length 2, coordinates for legend box
#' @param legend.main character, title for legend
#' @param legend.labels character, labels for legend boxes
#' @param main character, title for barplot
#' @param Rcssclass character, style class
plot.diffusion.barplot <- function(d, n=5, value.cols=c("value"),
                                   remove.self=TRUE, xlim=NULL,
                                   legend.pos=NA,
                                   legend.main="",
                                   legend.labels=c("before", "after"),
                                   main=NULL,
                                   Rcssclass=c()) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("diffusion", Rcssclass))
  barwidth <- RcssValue("diffusion", "barwidth", default=0.8)
  bw2 <- barwidth/2
  
  if (length(unique(d$input))!=1) {
    stop("d must contain a single value in column 'input'")
  }
  d <- as.data.frame(d, stringsAsFactors=FALSE)
  d.input <- d$input[1]

  # prepare input data
  # (perhaps clean out some features that overlap with the input)
  dvals <- apply(as.matrix(d[, value.cols, drop=FALSE]), 1, max)
  dorder <- order(dvals, decreasing=TRUE)
  mm <- d[dorder, , drop=FALSE]
  if (remove.self) {
    self.rows <- sapply(d$feature, function(x) {
      grepl(x, d.input)
    })
    mm <- mm[!self.rows, , drop=FALSE]
  }
  mm <- mm[1:min(nrow(mm), n), , drop=FALSE]

  if (is.null(xlim)) {
    xlim <- c(0, max(d$value))
  }
  ylim <- c(-n, bw2)
  
  # draw main plot features
  par()
  plot(xlim, ylim, xlim=xlim, ylim=ylim)
  if (identical(main, NULL)) {
    main <- paste0("Diffusion of '", d.input, "'")
  }
  mtext(side=3, main, Rcssclass="x")
  axis(3, labels=NA, line=0, Rcssclass="x")
  axis(3, lwd=0, Rcssclass="x")
  feature.y <- -seq(1, nrow(mm))+0.5
  axis(2, at=feature.y, label=mm$feature, lwd=0, Rcssclass="y")
  
  # draw bars
  mm.seq <- seq_len(nrow(mm))
  if (length(value.cols)==1) {
    rect(rep(0, nrow(mm)), -mm.seq+0.5+bw2,
         mm[[value.cols[1]]], -mm.seq+0.5-bw2) 
  } else {
    rect(rep(0, nrow(mm)), -mm.seq+0.5+bw2, mm[[value.cols[1]]], -mm.seq+0.5,
         Rcssclass=c(value.cols[1]))
    rect(rep(0, nrow(mm)), -mm.seq+0.5, mm[[value.cols[2]]], -mm.seq+0.5-bw2,
         Rcssclass=c(value.cols[2]))
  }
  # perhaps draw values for bars that go beyond xlim
  if (length(value.cols)==1) {
    items.overflow <- which(mm[[value.cols[1]]] > xlim[2])
    if (length(items.overflow)) {    
      text(xlim[2], feature.y[items.overflow],
           signif(mm[[value.cols[1]]][items.overflow], 2),
           Rcssclass="overflow")
    }
  }
  
  # legend made up of boxes
  if (!identical(legend.pos, NA)) {
    cols <- c(RcssValue("rect", "col", default="#000000", value.cols[1]),
              RcssValue("rect", "col", default="#000000", value.cols[2]))
    legend(legend.pos[1], legend.pos[2], legend.labels,
           title=legend.main, fill=cols)
  }

  invisible(mm)
}

