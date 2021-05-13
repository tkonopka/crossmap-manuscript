# plot functions for explanations of tau precision


#' draw a set name and its items
#'
#' @param x numeric, x coordinate
#' @param y numeric, y coordinate
#' @param w numeric, x-offset between set name and its items
#' @param setname character, name of set
#' @param setitems character, components in the set
#' 
add.target.set <- function(x, y, w, setname, setitems) {
  text(x, y, paste0(setname, ":"), Rcssclass="setname")
  temp <- paste0("(", paste(setitems, collapse=", "), ")")
  text(x+w, y, temp, Rcssclass="set")
}


#' draw a set of gene sets 
#'
#' @param y numeric, vertical positions for three lines
#' @param Rcssclass character, style class
#' 
#' @return 
schematic.targets <- function(Rcssclass=c()) {

  # construct sets using letters of the alphabet
  result <- list(four=list(), three=list())
  avoid.na <- function(x) { setdiff(x, NA) }
  for (i in 1:length(letters)) {
    set4 <- avoid.na(letters[i:(i+3)] )
    set3 <- avoid.na(letters[i:(i+2)] )
    lab4 <- paste0(letters[i], "4")
    lab3 <- paste0(letters[i], "3")
    if (length(set4)==4) {
      result$four[[lab4]] <- set4
    }
    if (length(set3)==3) {
      result$three[[lab3]] <- set3
    }
  }
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("targets", Rcssclass))
  widths <- RcssValue("targets", "widths", default=c(0.1, 0.9))
  offsets <- RcssValue("targets", "offsets", default=c(0.1, 0.2))

  # main plot area
  parplot(c(0, 1), c(0, 1))
  mtext(side=3, "Target sets", Rcssclass="main")
  roundedpolygon(c(0, 0, offsets[1],
                   1-offsets[1], 1, 1,
                   1, 1, 1-offsets[1],
                   0+offsets[1], 0, 0),
                 c(offsets[2], 0, 0,
                   0,0, offsets[2],
                   1-offsets[2], 1, 1,
                   1, 1, 1-offsets[2]),
                 p=0.5)

  # compute positions of the individual elements, then draw them
  y.pos <- c(offsets[2], 1-offsets[2])
  x.pos <- seq(offsets[1], 1-offsets[1], length=5)
  for (i in 1:2) {
    add.target.set(x.pos[i], y.pos[2], widths[1],
                   names(result$four)[i], result$four[[i]])
    add.target.set(x.pos[i], y.pos[1], widths[1],
                   names(result$three)[i], result$three[[i]])
  }
  text(x.pos[3], y.pos, "...", Rcssclass="setname")
  add.target.set(x.pos[4], y.pos[2], widths[1],
                tail(names(result$four), 1), rev(result$four)[[1]])
  add.target.set(x.pos[4], y.pos[1], widths[1],
                 tail(names(result$three), 1), rev(result$three)[[1]])
  
  # return all the sets altogether
  Reduce(c, result)
}



#' draw a grid area with a header
#'
#' (Fill the individual rows using add.tauprecision.row)
#'
#' @param nrow integer, number of rows in the grid
#' @param taus numeric vector, value of tau to print in a header line
#' @param Rcssclass character, style class
#' 
schematic.tauprecision <- function(nrow=4, taus=c(0, 0.5, 1),
                                  Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("tauprecision", Rcssclass))
  tau.width <- RcssValue("tauprecision", "tau.width", default=0.5)
  comment.width <- RcssValue("tauprecision", "comment.width", default=1.5)
  x.widths <- c(1, 1, 1, rep(tau.width, length(taus)), comment.width)
  
  xlim <- c(0, sum(x.widths))
  ylim <- c(-nrow-0.5, 0.5)
  parplot(xlim, ylim)

  # draw headers
  text(c(0.5, 1.5, 2.5, xlim[2]-comment.width/2), 0,
       c("Query", "Expected", "Output", "Comment"),
       Rcssclass="header")
  tau.x <- 3+(tau.width/2)
  for (i in 1:length(taus)) {
    i.label <- paste("tau =", taus[i])
    text(tau.x, 0, i.label, Rcssclass="header")
    tau.x <- tau.x + tau.width
  }

  # draw dividers
  lines(rep(c(xlim, NA), nrow), rep(-seq(1, nrow)+0.5, each=3),
        Rcssclass="divider")
  lines(rep(1, 2), ylim, Rcssclass="divider")
  lines(rep(2, 2), ylim, Rcssclass="divider")
  lines(rep(3, 2), ylim, Rcssclass="divider")
  lines(rep(xlim[2]-comment.width, 2), ylim, Rcssclass="divider")
}


#'
#' @param row integer, vertical position of example
#' @param query character, vector of items used in query (only for display)
#' @param outputs character vector, items in target.sets
#' @param taus numeric, values of tau
#' @param comment character, string to display as a comment
#' @param Rcssclass character, style class
#' 
add.tauprecision.example <- function(target.sets, row=1, query,
                                    outputs=c(),
                                    expected=c(),
                                    taus=c(0, 0.5, 1),
                                    comment="", 
                                    Rcssclass=c()) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("tauprecision", Rcssclass))
  lheight <- RcssValue("tauprecision", "lheight", default=0.1)
  tau.width <- RcssValue("tauprecision", "tau.width", default=0.5)
  comment.width <- RcssValue("tauprecision", "comment.width", default=1.5)
  
  x.widths <- c(1, 1, 1, rep(tau.width, length(taus)), comment.width)
  
  # draw the query set an outputs
  query.lab <- paste0("(", paste(query,collapse=", "), ")")
  text(0.5, -row, query.lab, Rcssclass="set")
  expected.lab <- paste0("(", paste(expected, collapse=", "), ")")
  text(1.5, -row, expected.lab, Rcssclass="set")
  outputs.lab <- paste0("(", paste(outputs, collapse=", "), ")")
  text(2.5, -row, outputs.lab, Rcssclass="set")
  
  for (i in seq_along(taus)) {
    i.tau <- tauprecision(outputs, expected, tau=taus[i], sets=target.sets)
    text(3+tau.width*(i-0.5), -row, signif(i.tau, 3), Rcssclass="value")
  }
  
  comment.x <- sum(x.widths)-comment.width/2
  text(comment.x, -row, comment, Rcssclass="comment")
}

