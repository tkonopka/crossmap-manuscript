# tools for assessing distributions from data.table groups


#' compute a summary of groups using quantiles
#'
#' @param d data table
#' @param by character vectors, columns in d that define a group
#' @param value.col character, column in d with numerical data
#' @param q character or numeric, labels for quantiles
#' @return data table with columns for by and specified quantiles
dt_quantiles <- function(d, by=colnames(d)[1], value.col=colnames(d)[2],
                        q=c("05", "25", "50", "75", "95")) {
  
  d <- d[ , c(by, value.col), with=FALSE]
  setnames(d, value.col, "_value")
  
  q.numeric <- as.numeric(q)/100
  q.labels <- paste0(value.col, "_q", q)
  
  dlist <- split(d, d[, by, with=FALSE])
  result <- list()
  for (i in seq_len(length(dlist))) {
    x <- dlist[[i]]
    if (nrow(x)>0) {
      values <- x[["_value"]]
      q.out <- matrix(quantile(values, p=q.numeric), ncol=length(q.numeric))
      colnames(q.out) <- q.labels
      result[[i]] <- cbind(x[1,], q.out)
      result[[i]][[paste0(value.col, "_mean")]] <- mean(values)
    } 
  }
  result <- rbindlist(result)
  result[["_value"]] <- NULL
  result
}
  
  
