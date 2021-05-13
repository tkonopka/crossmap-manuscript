# script part of CrossmapExploration.Rmd
# looking for hits in disease databases

# location on disk of
diseases.dir <- file.path(data.dir, "disease")
diseases.config.file <- file.path(diseases.dir,
                                 "config-disease-ic.yaml")
diseases.data.file <- file.path(diseases.dir, "orphanet.yaml.gz")



# load mappings from disease ids to names
if (!assignc("diseases.data.info")) {
  diseases.data <- read_yaml(diseases.data.file)
  diseases.data.info <- rbindlist(lapply(diseases.data, function(x) {
    data.table(id=x$metadata$id, name=x$data$title)
  }))
  diseases.data.info$name <- gsub("ç", "c", diseases.data.info$name)
  savec(diseases.data.info)
}


# define search configurations
if (!assignc("diseases.configs")) {
  diseases.diffusion.strings <- make.string.dicts(list(pathways=c(0, 1),
                                                      interactions=c(0, 1)))
  diseases.configs <- make.configs.table(
    list(targets="diseases",
         n=30,
         diffusion=names(diseases.diffusion.strings)),
    template=diseases.config.file)
  diseases.configs$diffusion <- as.character(
    diseases.diffusion.strings[diseases.configs$diffusion])
  diseases.configs$diffusion.label <- get.json.keys(diseases.configs$diffusion)
  savec(diseases.configs)
}


# perform searches with text queries
if (!assignc("diseases.results")) {
  make.diseases.results <- function(configs, queries) {
    result <- list()
    for (i in 1:nrow(configs)) {
      i.label <- configs$config.label[i]
      for (j in seq_along(queries)) {
        j.text <- gsub(" ", ".", queries[j])
        i.result <- run.crossmap(config=configs$config.file[i],
                                dataset="diseases",
                                data=j.text,
                                use.text=TRUE,
                                n=configs$n[i],
                                diffusion=configs$diffusion[i],
                                action="search")
        i.result$config.label <- i.label
        setcolorder(i.result, c("config.label", "id", "target", "distance"))
        result[[length(result)+1]] <- i.result
      }
    }
    result <- rbindlist(result)
    info <- copy(diseases.data.info)
    setnames(info, c("id", "name"), c("target", "target_name"))
    result <- merge(result, info, by="target", all.x=TRUE)
    setcolorder(result,
                c("config.label", "id", "target", "target_name", "distance", "rank"))
    result[order(config.label, id, rank)]
  }
  diseases.results <- make.diseases.results(diseases.configs,
                                           diseases.examples.text)
  savec(diseases.results)
}

