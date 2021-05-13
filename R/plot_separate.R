

#' a panel with just a title
plot.separateTitle <- function(v, position=c(0.5, 0.5), Rcssclass=c()) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("separate", Rcssclass))
  par()
  plot(0,0, xlim=c(0, 1), ylim=c(0, 1))
  text(position[1], position[2], as.character(v))
}

