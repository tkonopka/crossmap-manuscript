# plot functions for explanations of ontology translation and manual training


#' draw a data document
#'
#' @param pos character, content with positive weight
#' @param neg character, content with negative weight
#' @param main character, title for document
#' @param Rcssclass character, style class
#' 
schematic.manual.learning <- function(pos="", neg="", main="",
                                     widths=c(0.35, 0.65),
                                     padding=c(0.04, 0.3),
                                     Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("dataitem", Rcssclass))
  widths <- widths/(sum(widths)+(2*padding[1]))
  ypos <- c(1-padding[2], padding[2])
  xlim <- c(0, 1)
  ylim <- c(0, 1)
  
  # main plot area
  parplot(xlim, ylim, xlim=xlim, ylim=ylim)
  mtext(side=3, main, Rcssclass="main")
  roundedpolygon(c(0, 0, padding[1],
                   1-padding[1], 1, 1,
                   1, 1, 1-padding[1],
                   0+padding[1], 0, 0),
                 c(padding[2], 0, 0,
                   0,0, padding[2],
                   1-padding[2], 1, 1,
                   1, 1, 1-padding[2]),
                 p=0.5, Rcssclass="outer")

  # draw the contents -
  # one line for positive data and one line for negative data
  text(padding[1]+0, ypos, c("positive:", "negative:"), Rcssclass="label")
  text(padding[1]+widths[1], ypos, c(pos, neg), Rcssclass="content")
}




#' draw a table with query, expected and output terms
#'
#' @param df data frame with rows containing the following columns
#' id, name, expected, expected_name, target, target_name
#' @param widths numeric, three values indicating width of columns for
#' Query, Expected, and Output
#' @param show.columns character vector, choose the columns to display
#' @param score.column character, column in df with a score
#' @param score.label character, label to print in header
#' @param show.id logical, toggle showing the geneset id
#' @param main character, title printed on top of the table
#' @param show.dividers logical, display horizontal lines between rows
#' @param ylim numeric, leave NULL to fit the data frame
#' @param Rcssclass character, style class
#' 
plot.table.expout <- function(df, widths=c(1, 1, 1, 0.3),
                             show.columns=c("query", "expected", "output", "score"),
                             score.column=NA, score.label="",
                             show.id=TRUE,
                             main="", show.dividers=TRUE,
                             ylim=NULL,
                             Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("table", "expout", Rcssclass))
  offset <- RcssValue("table", "id.name.offset", default=0.15)

  # establish x-coordinates for all the columns
  widths <- widths[1:4]
  names(widths) = c("query", "expected", "output", "score")
  for (.x in c("query", "expected", "output", "score")) {
    if (!.x %in% show.columns) {
      widths <- widths[names(widths)!=.x]
    }
  }
  x.pos <- c(0, cumsum(widths))
  names(x.pos) <- c(names(widths), "_")
  xlim <- c(0, sum(widths))
  if (identical(ylim, NULL)) {
    ylim <- c(-nrow(df), 1)-0.5
  }
  
  parplot(xlim, ylim)
  mtext(side=3, main, Rcssclass="main")
  
  # draw the header and table structure
  if ("score" %in% show.columns) {
    score.pos = mean(tail(cumsum(widths), 2))    
    text(score.pos, 0, score.label, adj=c(0.5, 0.5), Rcssclass="header")
  }
  if ("query" %in% show.columns) {
    text(x.pos["query"], 0, "Query", Rcssclass="header")
  }
  if ("expected" %in% show.columns) {
    text(x.pos["expected"], 0, "Expected", Rcssclass="header")
  }
  if ("output" %in% show.columns) {
    text(x.pos["output"], 0, "Output", Rcssclass="header")
  }
  lines(xlim, rep(-0.5+offset/2, 2), Rcssclass="header")

  if (nrow(df)==0) {
    return()
  }
  
  # draw the content of the table
  headers <- c(query="Query", expected="Expected", output="Output")
  y.pos <- -seq_len(nrow(df))
  id.cols <- c(query="id", expected="expected", output="target")
  name.cols <- c(query="name", expected="expected_name", output="target_name")
  for (.x in names(widths)) {
    text(x.pos[.x], 0, headers[.x], Rcssclass="header")
    if (.x == "score") {
      .values <- round(df[[score.column]], 3)
      text(score.pos, y.pos, .values, Rcssclass=c("cell", "score"))
    } else {
      .ids <- df[[id.cols[.x]]]
      .names <- df[[name.cols[.x]]]
      if (show.id) {
        text(x.pos[.x], y.pos+offset, .ids, Rcssclass=c("cell", "id"))
        text(x.pos[.x], y.pos-offset, .names, Rcssclass=c("cell", "name"))
      } else {
        text(x.pos[.x], y.pos, .names, Rcssclass=c("cell", "name"))
      }
    } 
  }
  if (show.dividers) {
    lines(rep(c(xlim, NA), nrow(df)), rep(y.pos-0.5, each=3),
          Rcssclass="divider")
  }
  
}

