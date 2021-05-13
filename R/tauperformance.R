#


#' compute tauprecision for batches of predictions
#'
#' @param predictions data.table with id, target, a ranking value
#' @param expected data table with id, and ...
#' @param tau numeric, passed to tauprecision
#' @param n integer, number of predictions to pass onto tauprecision
#' @param sets list of sets
#' @param sort.by character, column in predictions that reveal top hits
#' @param group.by chracter, columns in predictions that determine groups
#' @param cores integer, number of cores for data.table
#'
#' @return data.table with id, tauprecision, tau
configs.tauprecision <- function(predictions, expected, tau=0, n=5, sets=NULL,
                                 sort.by="p.value",
                                 group.by=c("method", "config.label"),
                                 cores=detectCores()) {
  
  if (is.null(sets)) {
    stop("sets cannot be null\n")
  }
  missing <- setdiff(c("id", "target", "config.label", sort.by),
                     colnames(predictions))
  if (length(missing)>0) {
    stop(paste0(c("missing columns: ", missing), collapse=", "))
  }
  
  expected <- split(expected$target, expected$id)

  # helper to compute tauprecision for one benchmark
  # (picks top n prediction, compares with expected predictions)
  compute.tauprecision <- function(id, x) {
    x <- x[order(x[[sort.by]]), ][1:min(nrow(x), n)]
    tauprecision(x$target, expected[[id]], tau=tau, sets=sets)
  }
  
  old.cores <- setDTthreads(cores)
  result <- predictions[, list(tauprecision=compute.tauprecision(id, .SD)),
                          by=c(group.by, "id")]
  setDTthreads(old.cores)
  
  result$tau <- tau
  result$n <- n
  result
}

