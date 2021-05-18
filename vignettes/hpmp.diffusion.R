# sub-script of CrossmapTranslation.Rmd
# runs crossmap diffusion info on examples



if (!assignc("hpmp.diffusion.examples")) {
  # helper to collect diffused representations for text and docuemnts
  get.hpmp.diffusion.data <- function(configs, min.value=0.01) {
    result <- list()
    hp.ids <- grep("HP", hpmp.diffusion.ids, value=TRUE)
    mp.ids <- setdiff(hpmp.diffusion.ids, hp.ids)
    # extract diffusion for items in the db
    for (i in seq_len(nrow(configs))) {
      i.label <- configs$config.label[i]
      i.result <- run.crossmap.info(config=configs$config.file[i],
                                   dataset="targets",
                                   ids=mp.ids,
                                   features=hpmp.diffusion.text,
                                   diffusion=configs$diffusion[i],
                                   action="diffuse")
      i.result$config.label <- i.label
      setcolorder(i.result, c("config.label", "input", "feature", "value"))
      result[[i]] <- i.result[value>=min.value]
    }
    # extract diffusion for items from disk files
    for (i in seq_len(nrow(configs))) {
      i.label <- configs$config.label[i]
      for (hp.id in hp.ids) {
        hp.file <- file.path(hp.terms.dir, paste0(hp.id, ".yaml.gz"))
        hp.file <- gsub(":", "_", hp.file)
        i.result <- run.crossmap.info(config=configs$config.file[i],
                                     dataset="targets",
                                     data=hp.file,
                                     diffusion=configs$diffusion[i],
                                     action="diffuse")
        i.result$config.label <- i.label
        setcolorder(i.result, c("config.label", "input", "feature", "value"))
        result[[length(result)+1]] <- i.result[value>=min.value]
      }
    }
    rbindlist(result)
  }
  hpmp.diffusion.examples <- get.hpmp.diffusion.data(
    hpmp.configs[config.label %in% hpmp.viz$config.label],
    min.value=-100)
  savec(hpmp.diffusion.examples)
}


