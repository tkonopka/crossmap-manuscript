# helper functions for Crossmap.Rmd

#' create JSON string representation of dicionaries
#'
#' @param weightlist named list with numerical values
#' @param name.collapse character, used to construct names for the list
#' components
#'
#' @return named list with dictionaries
make.string.dicts <- function(weightlist, name.collapse=",") {
  temp <- expand.grid(weightlist)
  temp <- split(temp, 1:nrow(temp))
  result <- lapply(temp, function(x) {
    x[colSums(x)==0] <- NULL
    gsub("\\[|\\]", "", toJSON(x))
  })
  rnames <- sapply(temp, function(x) {
    paste(as.character(x), collapse=name.collapse)
  })
  names(result) <- rnames
  result
}

#' create string representations of non-zero components of dictionaries
#'
#' (this is a sister function to make.string.dicts)
#'
#' @param weightlist named list with numerical values
#' @param name.collapse character, used to contruct names for the list
#' components and separate labels
#'
#' @return named list with strings. Each string summarizes the nonzero
#' components
make.string.labels <- function(weightlist, name.collapse=",") {
  temp <- expand.grid(weightlist)
  temp <- split(temp, 1:nrow(temp))
  result <- lapply(temp, function(x) {
    x[colSums(x)==0] <- NULL
    paste(colnames(x), collapse=name.collapse)
  })
  rnames <- sapply(temp, function(x) {
    paste(as.character(x), collapse=name.collapse)
  })
  names(result) <- rnames
  result
}


#' create a data table with configurations based on all combindations 
#'
#' @param charlist named list of character vectors
#' @param label logical, when TRUE, output table witll contain a
#' config.label column
#' @param template character, path to configuration file
#' @param prefix character, prefix for new column in output table
#'
#' @return data table with all combinations of settings from charlist
make.configs.table <- function(charlist, label=TRUE, template=NULL,
                              prefix=c("config", "data")) {
  prefix <- match.arg(prefix)
  if (prefix == "config") {
    if (!"n" %in% names(charlist)) {
      charlist$n <- 1
    }
    if (!"diffusion" %in% names(charlist)) {
      charlist$diffusion <- 0
    }
  }
  result <- data.table(expand.grid(charlist))
  if (nrow(result)==0) {
    stop("configurations table is empty\n")
  }
  result.plain <- copy(result)
  if (!is.null(template)) {
    # create a column with paths to configuration files
    result$.file <- template
    for (i in seq_len(nrow(result))) {
      iconfig <- template
      for (a in colnames(result.plain)) {
        iconfig <- gsub(toupper(a), result.plain[[a]][i], iconfig)
      }
      result$.file[i] <- iconfig
    }
  }
  if (label) {
    # create a column containing a concatenated label
    if (prefix=="config") {
      result.plain$n <- paste0("n", result.plain$n)
      result.plain$diffusion <- paste0("diff", result.plain$diffusion)
    }
    result$.label <- apply(as.matrix(result.plain), 1,
                          paste, collapse="_")
    if (any(duplicated(result$.label))) {
      warnings("configuration labels are not unique\n")
    }
  }
  # replace column names with the desired prefix
  setnames(result, c(".file", ".label"),
           paste0(prefix, c(".file", ".label")))
  result
}



#' create a sparse representatation for a vector
#'
#' @param x numeric vector
#' @param value.col character, name for column containing values
#'
#' @return data table with a sparse vector representation of x
sparse <- function(x, value.col="value") {
  result <- data.table(idx=seq_along(x), val=x)
  result <- result[val != 0]
  setnames(result, "val", value.col)
  result  
}


#' find all keys from a json string
#'
#' @param x character, JSON-encoded vector
#' @param collapse character, to separate keys in output string
#' @importFrom jsonlite fromJSON
#' 
#' @return string with all keys
get.json.keys <- function(x, collapse=",") {
  result <- vapply(x, function(z) {
    obj <- fromJSON(z)
    paste(names(obj), collapse=collapse)
  }, character(1))
  names(result) <- names(x)
  result
}


#' find a value for a single key from a json string
#'
#' @param x character, JSON-encoded vector
#' @param key character, key to extract from x
#' 
#' @return value
get.json.val <- function(x, key, as.type = "numeric", default=0) {
  result <- vapply(x, function(z) {
    obj <- fromJSON(z)
    if (!key %in% names(obj) ) {
      return(as.character(default))
    }
    as.character(obj[[key]])
  }, character(1))
  if (as.type=="integer") {
    result <- as.integer(result)
  } else if (as.type=="numeric") {
    result <- as.numeric(result)
  }
  result
}

