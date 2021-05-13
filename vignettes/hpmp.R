# script part of CrossmapTranslation.Rmd
# translation of hp terms into mp terms

# location on disk of datasets
mp.dir <- file.path(data.dir, "mp-translation")
hp.terms.dir <- file.path(mp.dir, "hp.parents.terms")
mp.parents.file <- file.path(mp.dir, "mp-parents.yaml.gz")
curation.file <- file.path(mp.dir, "hpmp-manual.yaml")


# ############################################################################
# Load data files


# location of hp dataset
hp.data.files <- list(
  hp.parents = file.path(mp.dir, "hp-parents.yaml.gz")
)


if (!assignc("hp.data")) {
  make.hp.data <- function() {
    result <- lapply(hp.data.files, read_yaml)
    # write out hp elements into individual files
    for (hp.name in names(hp.data.files)) {
      hp.dir <- file.path(mp.dir, paste0(hp.name, ".terms"))
      if (!file.exists(hp.dir)) {
        dir.create(hp.dir)
        for (i in 1:length(result[[hp.name]])) {
          hp.term <- names(result[[hp.name]])[i]
          hp.term.file <- gsub(":", "_",
                               file.path(hp.dir, paste0(hp.term, ".yaml")))
          write_yaml(result[[hp.name]][i], file=hp.term.file)
          gzip(hp.term.file, overwrite=TRUE)
        }
      }
    }
    result    
  }
  hp.data <- make.hp.data()
  savec(hp.data)
}
if (!assignc("hp.data.info")) {
  hp.data.info <- lapply(hp.data, function(x) {
    item.name <- function(z) {
      z$data_pos$name
    }
    item.length <- function(z) {
      sum(nchar(as.character(z$data_pos)))
    }
    item.compressed <- function(z) {
      length(memCompress(as.character(z$data_pos)))
    }
    result <- data.table(id=names(x),
                         name=sapply(x, item.name),
                         item.length=sapply(x, item.length),
                         item.compressed_length=sapply(x, item.compressed))
    compression.ratio <- result$item.length / result$item.compressed_length
    result$item.compression_ratio <- signif(compression.ratio, 5)
    result$name.length <- nchar(result$name)
    result
  })
}
if (!assignc("mp.data.info")) {
  mp.data.info <- read_yaml(mp.parents.file)
  mp.data.info <- rbindlist(lapply(mp.data.info, function(x) {
    data.table(id=x$metadata$id, name=x$data_pos$name)
  }))
  mp.data.info$name.length <- nchar(mp.data.info$name)
  savec(mp.data.info)
}


# established mappings from hp to mp
if (!assignc("hpmp.owlsim")) {
  owlsim.file <- file.path(mp.dir, "hp-mp-oomap.tsv.gz")
  hpmp.owlsim <- fread(owlsim.file)
  hpmp.owlsim <- hpmp.owlsim[, list(term2=term2, score=score, ties=length(score)-1),
                               by=c("term1")]
  savec(hpmp.owlsim)
}
if (!exists("hpmp.owlsim.difficult.ids")) {
  hpmp.owlsim.difficult.ids <- hpmp.owlsim[ties>0]$term1
}


# manual associations
if (!assignc("hpmp.manual.data")) {
  hpmp.manual.data <- read_yaml(curation.file)
}
if (!assignc("hpmp.manual.attempted.ids")) {
  # HP ids attempted to be adjusted are recorded in the metadata
  hpmp.manual.attempted.ids <- vapply(hpmp.manual.data, function(x) {
    result <- strsplit(x$metadata$comment, " ")[[1]]
    result[grep("HP", result)]
  }, character(1))
  names(hpmp.manual.attempted.ids) <- NULL
}



# ############################################################################
# Run mappings and evaluations


if (!assignc("hpmp.configs")) {
  hpmp.diffusion.strings <- make.string.dicts(list(targets=seq(0, 0.28, by=0.04),
                                                   manual=seq(0, 2.8, by=0.4)))
  hpmp.config.template <- file.path(mp.dir, "config-mp-translation-TARGETS.yaml")
  hpmp.configs <- make.configs.table(
    list(targets="parents",
         documents="wiktionary_10",
         n=5,
         diffusion=names(hpmp.diffusion.strings)),
    template=hpmp.config.template)
  hpmp.configs <- hpmp.configs[!documents %in% c("none"), ]
  # replace diffusion labels by dictionaries
  hpmp.configs$diffusion <- as.character(
    hpmp.diffusion.strings[hpmp.configs$diffusion])
  hpmp.configs$diffusion.label <- get.json.keys(hpmp.configs$diffusion)
  hpmp.configs$diff_targets <- sapply(hpmp.configs$diffusion, get.json.val, "targets")
  hpmp.configs$diff_manual <- sapply(hpmp.configs$diffusion, get.json.val, "manual")
  hpmp.configs <- hpmp.configs[!(documents %in% c("none") &
    grepl("documents", diffusion)),]
  # avoid using diffusion along targets and documents at once
  hpmp.configs <- hpmp.configs[!(grepl("targets", diffusion) &
    grepl("documents", diffusion))]
  savec(hpmp.configs)
}


# run and collect information
if (!assignc("hpmp.results")) {
  make.hpmp.results <- function(cores=detectCores()) {
    result <- list()
    for (i in names(hp.data.files)) {
      temp <- configsrun.search(hpmp.configs,
                                hp.data.files[[i]],
                                data.label=i,
                                cores=cores)
      result[[i]] <- merge(temp, hp.data.info[[i]], by="id")
    }
    rbindlist(result)
  }  
  hpmp.results <- make.hpmp.results(cores=3)
  savec(hpmp.results)
}


# match hp terms to mp terms by name only
# (This goes by brute force and can take some time)
if (!assignc("hp.namematch")) {
  make.hp.namematch <- function(k=5) {
    # get hp and mp names, split into kmers
    hp.hits <- grep("^HP", onto$names$id)
    hp.names <- setNames(onto$names$name[hp.hits], onto$names$id[hp.hits])
    mp.hits <- grep("^MP", onto$names$id)
    mp.names <- setNames(onto$names$name[mp.hits], onto$names$id[mp.hits])
    hp.kmers <- lapply(lapply(hp.names, tolower), kmers, k=k)
    mp.kmers <- lapply(lapply(mp.names, tolower), kmers, k=k)
    # construct best matches
    result <- rbindlist(mclapply(seq_along(hp.names), function(i) {
      x <- hp.kmers[[i]]
      x.ji <- sapply(mp.kmers, ji, x)
      best <- which.max(x.ji)[1]
      data.table(id=names(hp.names)[i], name=hp.names[i],
                 best.mp.id=names(mp.names)[best],
                 best.mp.name=mp.names[best],
                 best.mp.ji=signif(x.ji[best], 4))
    }, mc.cores=detectCores()))
    setcolorder(result, c("id", "name", "best.mp.id", "best.mp.name"))
    result
  }
  hp.namematch <- make.hp.namematch()
  savec(hp.namematch)
}


# evaluate output (item-by-item level)
if (!assignc("hpmp.evaluations")) {
  # compare hpmp.results against a set of expected pairings
  make.hpmp.evaluations <- function(expected, idexp.cols=c("term1", "term2"),
                                    evaluation.label="") {
    expected <- copy(expected)[, idexp.cols, with=FALSE]
    setnames(expected, idexp.cols, c("id", "expected"))
    result <- evaluate.ontology.mapping(hpmp.results, expected,
                                        onto, by=c("config.label", "data.label"))
    result <- merge(result,
                    unique(hpmp.results[, c("id", "data.label",
                                            "item.length",
                                            "item.compressed_length",
                                            "item.compression_ratio")]),
                    by=c("id", "data.label"))
    # when there are multiple expected items, pick the best (precision, pathlen)
    result <- result[order(-precision_bestN, pathlen_bestN)]
    result <- result[!duplicated(result[, c("id", "config.label", "data.label")])]
    temp <- hp.namematch[, c("id", "best.mp.ji")]
    setnames(temp, "best.mp.ji", "name.match.ji")
    result <- merge(result, temp, by="id", all.x=TRUE)
    result$eval.label <- evaluation.label
    result[order(id)]
  }
  # compare against owlsim calculations
  # and against simple lexical matching (based on name only) 
  hpmp.evaluations <- rbind(
    make.hpmp.evaluations(hpmp.owlsim, c("term1", "term2"), "owlsim"))
  # make.hpmp.evaluations(hp.namematch, c("id", "best.mp.id"), "lexical"))
  savec(hpmp.evaluations)
}


# create a one-line summary of performance of each scenario
if (!assignc("hpmp.summary")) {
  hpmp.summary <- lapply(c("precision", "precision_bestN",
                           "pathlen", "pathlen_bestN", "pathlen_sumN"),
                         function(x) {
                           dt_quantiles(hpmp.evaluations,
                                        by=c("config.label", "data.label", "eval.label"),
                                        value.col=x)
                         })
  hpmp.summary <- Reduce(merge, hpmp.summary)
  savec(hpmp.summary)
}


# evaluate which of the configurations give the best performance
if (!assignc("hpmp.evaluations.summary")) {
  hpmp.evaluations.summary <-
    hpmp.evaluations[ , list(precision=mean(precision),
                             precision_bestN=mean(precision_bestN),
                             pathlen=mean(pathlen),
                             pathlen_bestN=mean(pathlen_bestN))
      , by=c("data.label", "config.label", "eval.label")]
  hpmp.evaluations.summary <- merge(hpmp.evaluations.summary,
                                    hpmp.configs[, c("config.label", "diffusion.label",
                                                     "diff_targets", "diff_manual")],
                                    by="config.label")
  hpmp.evaluations.summary <- hpmp.evaluations.summary[order(-precision_bestN)]
  savec(hpmp.evaluations.summary)
}
if (!assignc("hpmp.best.config")) {
  hpmp.best.config <- copy(hpmp.evaluations.summary[1, ])
}
if (!assignc("hpmp.best.auto.config")) {
  hpmp.best.auto.config <- copy(hpmp.evaluations.summary[diffusion.label=="targets"][1, ])
}


# For preparing manual curation, obtain a subset of HP items
if (!assignc("hpmp.selection")) {
  select.hpmp.pairs <- function(conf, name.match.range=c(0, 0.5),
                                pathlen.threshold=8) {
    items <- merge(conf[, c("config.label", "diffusion.label", "eval.label")],
                   hpmp.evaluations,
                   by=c("config.label", "eval.label"))
    items <- items[!id %in% hpmp.owlsim.difficult.ids]
    nmr <- name.match.range
    if (is.na(nmr[1])) { nmr[1] <- -1 }
    if (is.na(nmr[2])) { nmr[2] <- 2 }
    items <- items[name.match.ji>nmr[1] & name.match.ji<=nmr[2]]
    items <- items[pathlen_bestN>=pathlen.threshold]
    items <- items[order(pathlen_bestN, decreasing=TRUE)]
    return(items)
  }
  hpmp.selection <- select.hpmp.pairs(hpmp.best.auto.config, name.match.range=c(NA, NA),
                                      pathlen.threshold=8)
  fwrite(hpmp.selection, file=file.path(results.dir, "hpmp.selection.tsv"), sep="\t")
}

if (!exists("hpmp.focus.ids")) {
  hpmp.focus.ids <- unique(hpmp.selection$id)
}


# For visualizations, prepare tables on just selected configurations
#
# plain - no diffusion at all
# diffused - diffused without manual annotations
# manul - diffused only with manual annotations
# best - diffused with automatic & manual annotations
if (!assignc("hpmp.viz")) {
  # construct dataset 
  make.hpmp.viz <- function() {
    result <- list()
    best.config <- hpmp.best.config$config.label
    data.label <- hpmp.best.config$data.label
    eval.label <- hpmp.best.config$eval.label
    # helper to extract data for one configuration
    .extract.data <- function(extract.config, data.label, viz.label="") {
      result <- data.table(config.label=extract.config,
                           data.label=data.label,
                           eval.label=eval.label)
      result <- merge(hpmp.evaluations,
                      result[, c("config.label", "data.label", "eval.label")],
                      by=c("config.label", "data.label", "eval.label"))
      result$viz <- viz.label
      # collapse down to one row per "id", aka HP term
      # (some HP id may have multiple rows if owlsim gives ties)
      result[order(pathlen_bestN)][!duplicated(id)]
    }
    #
    # undiffused, plain translation
    plain.config <- hpmp.configs[diffusion=="{}"]$config.label
    result$plain <- .extract.data(plain.config, data.label, viz.label="plain")
    # diffused, without manual
    diffused.config <- unlist(strsplit(best.config, ","))
    diffused.config[length(diffused.config)] = "0"
    diffused.config <- paste(diffused.config, collapse=",")
    result$diffused <- .extract.data(diffused.config, data.label, "diffused")
    # diffused, only manual
    manual.config <- unlist(strsplit(best.config, "diff|,"))
    manual.diff <- tail(manual.config, 1)
    manual.diff <- c(rep("0", length(manual.config)-2), manual.diff)
    manual.config <- paste0(manual.config[1], "diff", paste(manual.diff, collapse=","))
    result$manual <- .extract.data(manual.config, data.label, "manual")
    # best configuration, diffused and manual
    result$best <- .extract.data(best.config, data.label, "best")
    rbindlist(result)
  }
  hpmp.viz <- make.hpmp.viz()
  savec(hpmp.viz)
}

