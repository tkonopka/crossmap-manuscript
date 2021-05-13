#


#' jaccard index from two vectors
#'
#' @param a vector, assumed to have unique elements
#' @param b vector, assumed to have unique elements
#'
#' @return numeric, jaccard index of the two sets
ji <- function(a, b) {
  au <- unique(a)
  common <- sum(au %in% b)
  result <- common / length(unique(c(au, b)))
  if (!is.finite(result)) {
    result <- 0
  }
  result
}



#' tau-precision assessment of how similar predictions are to an
#' expected outcome
#'
#' @param predictions list or vector of predictions, order is important
#' @param expected list or vector of expected items, order unimportant
#' @param tau numeric, precision decay parameter for multiple hits
#' @param sets list of vectors, used when predictions and expected are
#' provided as vectors
#' 
#' @return numeric
tauprecision <- function(predictions, expected, tau=0, sets=NULL) {
  
  # checks on arguments, inputs
  if (!is.list(predictions) | !is.list(expected)) {
    if (is.null(sets) | !is.list(sets)) {
      stop.msg <- c("predictions and expected must be lists,",
                   "or sets must be provided\n")
      stop(paste(stop.msg, collapse=" "))
    }
  }

  # prepare all inputs into lists
  if (!is.list(predictions)) {
    predictions <- sets[predictions]
  }
  if (!is.list(expected)) {
    expected <- sets[expected]
  }

  # main calculation
  if (length(predictions)==0 | length(expected)==0) {
    return (0)
  }
  weights <- rep(1, length(expected))
  result <- 0
  for (i in seq_along(predictions)) {
    i.set <- predictions[[i]]
    similarities <- sapply(expected, ji, i.set)
    hit <- which.max(similarities)
    result <- result + (similarities[hit]*weights[hit])
    weights[hit] <- weights[hit]*tau
  }
  as.numeric(result)
}

