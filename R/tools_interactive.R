# tools for interactive investigations


#' execute crossmap info commands to get compare feature values
#'
#' @param config character, path to configuration file
#' @param data.file character, path to a disk file
#' @param diffusion character, JSON string representation of a dictionary
#'
#' @return data table
get.feature.values <- function(config=NULL, data.file=NULL,
                               diffusion="{\"targets\":1}") {
  
  diffusion <- add.single.quotes(nospace.json(diffusion))
  crossmap.cmd <- c(crossmap.executable, "diffuse",
                      "--config", config, "--logging ERROR")
  crossmap.nodiff <- c(crossmap.cmd, "--diffusion", "{}")
  crossmap.diff <- c(crossmap.cmd, "--diffusion", diffusion)

  # run crossmap on the disk file
  run <- function(cmd.vector, value.column="value") {
    cmd <- paste(cmd.vector, collapse=" ")
    result <- fromJSON(system(cmd, intern=TRUE))
    result <- as.data.table(result)
    setcolorder(result, c("input", "feature", "value"))
    setnames(result, "value", value.column)
  }

  data.nodiff <- run(c(crossmap.nodiff, "--data", data.file), "plain")
  data.diff <- run(c(crossmap.diff, "--data", data.file), "diff")
  
  result <- merge(data.nodiff, data.diff,
                 by=c("input", "feature"), all=TRUE)
  result[is.na(result)] = 0
  result
}

