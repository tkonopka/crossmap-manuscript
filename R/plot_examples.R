# plot hit results for example queries


shorten_strwidth <- function(x, maxwidth, cex=0.85) {
  if (length(x)>1) {
    result <- sapply(x, shorten_strwidth, maxwidth, cex=cex)
    return(result)
  }
  x.widths <- sapply(1:nchar(x), function(z) {
    strwidth(substr(x, 1, z), cex=cex)
  })
  if (min(x.widths)>maxwidth) {
    result <- 1
  } else {
    threedots <- strwidth("...", cex=cex)
    result <- max(which(x.widths < maxwidth-threedots))
  }
  if (result == nchar(x)) return(x)
  paste0(substr(x, 1, result), "...")
}


#' draw a custom table with query, diffusion settings, hit results
#'
#' @param queries character vector of length 2, 
#' @param diffusion character vector of length 1 or 2
#' @param hits list with two data tables, each with id and name columns
#' @param num.hits integer, number of hits to write out
#' @param maxlen integer, number of characters to include when listing hits
#' @param colwidths numeric vector, widths of columns
#' @param Rcssclass character, style class
#'
plot.examples <- function(queries=c("text", "text"),
                         diffusion=c("-", "abc"),
                         hits = list(A=data.table(id=letters[1:2], name=letters[1:2]),
                                     B=data.table(id=letters[1:5], name=letters[1:5])),
                         num.hits=8,
                         colwidths=c(0.12,0.44,0.44), spacing=0.03,
                         Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("examples", Rcssclass))
  offset <- RcssValue("table", "id.name.offset", default=0.15)
  name.cex <- RcssValue("text", "cex", default=1, Rcssclass=c("cell", "name"))
  xlim <- c(0, sum(colwidths)+2*spacing)
  
  parplot(c(0, 1), c(0, 1),
          xlim=xlim, ylim=c(-num.hits, 2),
          type="n", frame=FALSE,
          xlab="", ylab="", main="", axes=FALSE)
  
  col.x <- c(0, colwidths[1]+spacing, colwidths[1]+colwidths[2]+2*spacing)
  
  # draw labels for left-hand column
  text(0, 1.5, "query", Rcssclass="lhs")
  text(0, 0.5, "diffusion", Rcssclass="lhs")
  text(0, -0.5, "hits", Rcssclass="lhs")
  lines(xlim, c(1, 1), Rcssclass="horizontal")
  lines(xlim, c(0, 0), Rcssclass="horizontal")
  lines(c(col.x[2], col.x[2])-spacing/2, c(-num.hits, 2), Rcssclass="vertical")

  # draw the queries and diffusion
  add.settings <- function(settings, y, Rcssclass="settings") {
    if (length(settings)==1) {
      text((xlim[2]+col.x[2])/2,
           y, settings, Rcssclass=Rcssclass)
    } else {
      text(c((col.x[2]+col.x[3])/2, (xlim[2]+col.x[3])/2),
           y, settings, Rcssclass=Rcssclass)
    }
  }
  add.settings(queries, 1.5, Rcssclass="query")
  add.settings(diffusion, 0.5, Rcssclass="diffusion")
  
  # draw the hits
  add.examples.hits <- function(hits, x, y, width=0.5) {
    name.text <- shorten_strwidth(hits$name, width, cex=name.cex)
    for (i in seq_len(nrow(hits))) {
      if (is.na(hits$id[i])) {
        text(x[i], y[i], hits$name[i], Rcssclass=c("cell", "others"))
      } else {
        text(x[i], y[i]+offset, hits$id[i], Rcssclass=c("cell", "id"))
        text(x[i], y[i]-offset, name.text[i], Rcssclass=c("cell", "name"))
      }
    }
  }
  head.hits <- function(hits.tab) {
    result = hits.tab[, c("id", "name")]
    if (nrow(hits.tab)>num.hits) {
      result <- head(hits.tab, num.hits-1)[, c("id", "name")]
      result <- rbind(result, data.table(id=NA, name="... (other ranked hits)"))
    }
    result
  }
  hits.1 <- head.hits(hits[[1]])
  add.examples.hits(hits.1,
                    rep(col.x[2], nrow(hits.1)),
                    -seq(0.5, nrow(hits.1)-0.5),
                    width=colwidths[2])
  if (length(hits)>1) {
    hits.2 <- head.hits(hits[[2]])
    add.examples.hits(hits.2, rep(col.x[3], nrow(hits.2)),
                      -seq(0.5, nrow(hits.2)-0.5),
                      width=colwidths[3])
  }
}


