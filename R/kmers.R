

#' split a long string into kmers (brute force)
#'
#' (this is not the function used in the python crossmap software!)
#'
#' @param x character string
#' @param k integer, length of kmers
#' @return character vector
kmers <- function(x, k=4) {
  xlen <- nchar(x)
  if (nchar(x)<=k) { return(x) }
  sapply(1:(xlen-k+1), function(i) { substr(x, i, i+k-1) })
}


#' jaccard index of two strings based on kmers
#' @param a character vector
#' @param b character vector
#' @param k integer, length of kmers
#'
#' @return numeric vector of same length as a
kmer.ji <- function(a, b, k=4) {
  alen <- length(a)
  result <- rep(NA, alen)
  for (i in seq_len(alen)) {
    result[i] <- ji(kmers(a[i], k=k), kmers(b[i], k=k))
  }
  result
}
