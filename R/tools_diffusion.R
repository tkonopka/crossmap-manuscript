

#' create a side-by-side comparison of feature values from two configurations
#'
#' @param d data table with column 'config.label' and others
#' @param pair character, two items in config.label
#' @param pair.labels character
#'
#' @return data table with columns input, feature, and two values
get.diffusion.comparison <- function(d, pair=c("A", "B"),
                                    pair.labels=c("A", "B")) {
  value.columns <- paste0("value_", pair.labels)
  valA <- d[config.label==pair[1]]
  valA$config.label <- NULL
  setnames(valA, "value", value.columns[1])
  valB <- d[config.label==pair[2]]
  valB$config.label <- NULL
  setnames(valB, "value", value.columns[2])
  result <- merge(valA, valB, by=c("input", "feature"), all=TRUE)
  result[is.na(result)] <- 0
  vc <- value.columns
  result[order(result[[vc[1]]], result[[vc[2]]], decreasing=TRUE), ]
}

