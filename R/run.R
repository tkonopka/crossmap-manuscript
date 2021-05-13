# tools for runnning crossmap analysis


#' ensure that an input is a simple (unboxed) JSON string
#'
#' @param x list or character
#' @return converts a list into JSON string
nospace.json <- function(x) {
  if (class(x)=="character") {
    return(x)
  }
  gsub(" ", "", toJSON(x, auto_unbox=TRUE))
}


#' ensure that a string starts and ends with single quotes
#' @param x character (one string)
#' @return character string that starts and ends with a single quote
add.single.quotes <- function(x) {
  if (!startsWith(x, "'")) {
    x <- paste0("'", x)
  }
  if (!endsWith(x, "'")) {
    x <- paste0(x, "'")
  }
  x
}


#' execute a crossmap search/decompose command
#'
#' @param config character, path to a configuration file
#' @param data character, path to a data file with documents
#' @param n integer, number of nearest neighbors
#' @param logging character, logging level
#' @param diffusion character, JSON string representation of a dictionary
#' (diffusion strength)
#' @param action character, use one of the options provided
#' @param dataset character, name of dataset to query
#' @param use.text logical, set TRUE to run usig --text as opposed to --data
#'
#' @return list with output
run.crossmap <- function(config=NULL, data=NULL, n=1, logging="ERROR",
                         diffusion="{}", action=c("search", "decompose"),
                         dataset="targets", use.text=FALSE) {
  action <- match.arg(action)
  diffusion <- add.single.quotes(nospace.json(diffusion))
  crossmap.cmd <- c(crossmap.executable, action,
                    "--config", config,
                    "--dataset", dataset,
                    "--logging", logging,
                    "--n", n,
                    "--diffusion", diffusion)
  if (use.text) {
    crossmap.cmd <- c(crossmap.cmd, "--text", data)
  } else {
    crossmap.cmd <- c(crossmap.cmd, "--data", data)
  }
  crossmap.cmd <- paste(crossmap.cmd, collapse=" ")
  print(paste(date(), crossmap.cmd))
  raw <- fromJSON(system(crossmap.cmd, intern=TRUE))
  result <- lapply(split(raw, raw$query), function(x) {
    xhits <- length(x$targets[[1]])
    if (xhits==0) { return(NULL) }
    data.table(id=x$query,
               target=x$targets[[1]],
               distance=x$distances[[1]],
               coefficient=x$coefficients[[1]],
               rank=seq_along(x$targets[[1]]))
  })
  rbindlist(result)
}


#' execute crossmap runs on several configurations
#'
#' I attempted to parallelize this by launching many runs through mclapply.
#' However, as some of the configuration point to the same files and dbs,
#' there seemed to be contention and not much speedup.
#'
#' @param configstab data table with configurations
#' @param inputfile path to an input file
#' @param data.label character, a label to associate to the output table
#' @param action character, passed to run.crossmap.action
#' @param dataset character, name of dataset to query
#' @param cores integer, number of jobs to launch at once
#'
#' @return data table with outputs from all configuration on the input file
configsrun.crossmap <- function(configstab, inputfile, data.label="",
                                action="search", dataset="targets", cores=1) {
  missing <- setdiff(c("config.file", "config.label"), colnames(configstab))
  if (length(missing)>0) {
    stop("missing column in configstab: ", paste(missing, collapse=", "), "\n")
  }
  tab <- copy(configstab)
  tab$data.label <- data.label
  result <- mclapply(split(tab, 1:nrow(tab)), function(x) {
    temp <- run.crossmap(x$config.file, inputfile,
                         n=x$n,
                         diffusion=x$diffusion,
                         action=action,
                         dataset=dataset)
    temp$config.label <- x$config.label
    temp$data.label <- x$data.label
    temp
  }, mc.cores=cores)
  result <- rbindlist(result)
  if (data.label == "") {
    result$data.label <- NULL
  }
  result
}


#' wrapper for configsrun.action with action="search"
#' @param ... all arguments passed on to configsrun.crossmap
#' @return output of configsrun.action
configsrun.search <- function(...) {
  configsrun.crossmap(..., action="search")
}

#' wrapper for configsrun.action with action="decompose"
#' @param ... all arguments passed on to configsrun.crossmap
#' @return output of configsrun.action
configsrun.decompose <- function(...) {
  configsrun.crossmap(..., action="decompose")
}


#' execute a crossmap command to fetch information about identifiers/features
#'
#' @param config character, path to a configuration file
#' @param data character, 
#' @param ids character vector, ids to query
#' @param features character vector, text (for diffuse)
#' @param data character, path to file
#' @param diffusion character, JSON string representation of a dictionary of
#' diffusion strengths
#' @param logging character, logging level
#' @param action character, use one of the options provided
#'
#' @return list with output
run.crossmap.info <- function(config=NULL, dataset=NULL, ids=NULL,
                              features=NULL, data=NULL,
                              diffusion="{}", logging="ERROR",
                              action=c("vectors", "diffuse", "matrix", "distances")) {
  action <- match.arg(action)
  diffusion <- add.single.quotes(nospace.json(diffusion))
  crossmap.cmd <- c(crossmap.executable, action,
                    "--config", config,
                    "--logging", logging,
                    "--diffusion", diffusion)
  if (!is.null(ids)) {
    crossmap.cmd <- c(crossmap.cmd, "--ids", paste(ids, collapse=","))
  }
  if (!is.null(features)) {
    crossmap.cmd <- c(crossmap.cmd, "--text", paste(features, collapse=","))
  }
  if (!is.null(dataset)) {
    crossmap.cmd <- c(crossmap.cmd, "--dataset", dataset)
  }
  if (!is.null(data)) {
    crossmap.cmd <- c(crossmap.cmd, "--data", data)
  }
  crossmap.cmd <- paste(crossmap.cmd, collapse=" ")
  raw <- fromJSON(system(crossmap.cmd, intern=TRUE))
  
  if (action=="vectors") {
    result <- list()
    for (i in seq_along(raw$id)) {
      id <- raw$id[i][[1]]
      vec <- raw$vector[i][[1]]
      result[[id]] <- vec
    }
    result <- as.data.table(result)
    result$"_index" <- seq_len(nrow(result))-1
    setcolorder(result, c("_index", ids))
  } else if (action=="diffuse") {
    result <- as.data.table(raw)
    setcolorder(result, c("input", "feature", "value"))
    result <- result[order(value, decreasing=TRUE)]
  } else if (action=="matrix") {
    result = as.data.table(raw)
    setcolorder(result, c("_feature", "_max", "_min"))
  } else if (action=="distances") {
    result <- as.data.table(raw)
  }
  result  
}


#' execute multiple crossmap.info runs for different configurations
#'
#' @param configstab table with configurations
#' @param min.value numeric, threshold
#' @param diffusion.text character, input to diffuse 
configsrun.diffusion <- function(configstab, min.value=0.01, features="text") {
  result <- list()
  for (i in seq_len(nrow(configstab))) {
    i.label <- configstab$config.label[i]
    i.result <- run.crossmap.info(config=configstab$config.file[i],
                                  dataset="positions",
                                  ids=NULL,
                                  features=features,
                                  diffusion=configstab$diffusion[i],
                                  action="diffuse")
    i.result$config.label <- i.label
    setcolorder(i.result, c("config.label", "input", "feature", "value"))
    result[[i]] <- i.result[value>=min.value]
  }
  rbindlist(result)
}

