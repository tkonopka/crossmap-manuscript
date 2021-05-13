# sub analysis of Crossmap.Rmd
# evaluation of gene sets from HAP1/eHAP cell lines


# location on disk of geneset datasets
genesets.dir <- file.path(data.dir, "hsapiens-genesets")
find.expression.file <- function(x) {
  x2 <- file.path(data.dir, c("ehap", "gdsc", "hsapiens-genesets"), x)
  x2[file.exists(x2)][1]
}
ehap.DE.raw.file <- find.expression.file("expression-eHAP-DE-table.tsv.gz")
ehap.DE.file <- find.expression.file("expression-eHAP.yaml.gz")
genesets.config.file <- find.expression.file("config-hsapiens-genesets-uniform.yaml")
genesets.superset.file <- find.expression.file("hsapiens-superset.yaml.gz")


if (!assignc("expression.configs")) {
  diffusion.weights <- list(interactions=c(0, 0.2),
                            pathways=c(0, 0.2),
                            tissues=c(0, 0.2),
                            positions=c(0, 0.2))
  diffusion.strings <- make.string.dicts(diffusion.weights)
  diffusion.labels <- make.string.labels(diffusion.weights)
  expression.configs <- make.configs.table(
    list(n=5,
         diffusion=names(diffusion.strings)),
    label=TRUE, template=genesets.config.file,
    prefix="config")
  expression.configs$diffusion.label <- as.character(
    diffusion.labels[expression.configs$diffusion])
  expression.configs$diffusion <- as.character(
    diffusion.strings[expression.configs$diffusion])
  # look only at diffusion from one dataset at a time
  expression.configs <- expression.configs[!grepl(",", diffusion)]
  for (.diffusion in names(diffusion.weights)) {
    expression.configs[[paste0("diffusion.strength.", .diffusion)]] <-
      sapply(expression.configs$diffusion, get.json.val, .diffusion)
  }
  rm(diffusion.strings, diffusion.labels, diffusion.weights, .diffusion)
  savec(expression.configs)
}

# column in expression configs with numeric values for diffusion strengths
expression.configs.strengthcols <- grep("diffusion.strength",
                                        colnames(expression.configs), value=T)

if (!exists("diffusion.configs")) {
  diffusion.configs <- expression.configs[, c("config.label", "diffusion.label",
                                              expression.configs.strengthcols),
                                            with=FALSE]
  diffusion.configs <- unique(diffusion.configs)
}


# load the target genesets and fetch their titles
if (!assignc("targets.genesets") | !assignc("targets.info")) {
  superset <- read_yaml(genesets.superset.file)
  # make a list with gene sets
  targets.genesets <- lapply(superset, function(x) { x$data$symbols })
  names(targets.genesets) <- gsub("BP_|MF_|CC_", "", names(superset))
  savec(targets.genesets)
  # table mapping gene set id to a description
  targets.info <- data.table(
    target=names(targets.genesets),
    target_name=sapply(superset, function(x) { x$title }))
  savec(targets.info)
  rm(superset)
}


# table with all ehap results - genes, z-scores, fc
if (!exists("ehap.DE.table")) {
  ehap.DE.table <- fread(ehap.DE.raw.file)
  ehap.DE.table$position <- (ehap.DE.table$start + ehap.DE.table$end)/2
}


# collect information about gdsc cell lines and ehap cell lines
if (!assignc("expression.genesets") | !assignc("expression.info")) {
  # collect information about eHAP cell line
  ehap.items <- read_yaml(ehap.DE.file)
  # make a list with gene sets
  expression.genesets <- lapply(ehap.items, function(x) { x$data$genes })
  expression.genesets <-
    expression.genesets[sapply(expression.genesets, length)>0]
  savec(expression.genesets)
  # make a summary table
  expression.info <- lapply(ehap.items, function(x) {
    xmeta <- x$metadata
    xnum <- length(unlist(x$data))
    if ("values" %in% names(x)) {
      xnum <- length(x$values)
    }
    data.table(id=xmeta$id,
               name=x$title,
               group=xmeta$cell_line,
               method="z",
               criterion=xmeta$z,
               collection="Haplogen",
               num_genes=xnum,
               direction=xmeta$direction)
  })
  expression.info <- unique(rbindlist(expression.info))
  rm(ehap.items)
  savec(expression.info)
}


#' run crossmap on ehap datasets, looking in position and superset genesets
ehap.run <- function(action="search",
                     dataset=c("superset", "positions", "processes")) {
  expconfigs <- expression.configs
  result <- rbindlist(lapply(dataset, function(d) {
    temp <- configsrun.crossmap(expconfigs, ehap.DE.file, dataset=d,
                                cores=detectCores(), action=action)
    temp$dataset <- d
    temp
  }))
  result$target <- gsub("BP_|MF_|CC_", "", result$target)
  result <- merge(result, targets.info, by="target", all.x=TRUE)
  setcolorder(result, c("id", "target", "target_name",
                        "rank", "dataset", "config.label"))
  result[order(id, config.label, rank)]
}
if (!assignc("ehap.results")) {
  ehap.results <- list(search=ehap.run(action="search"),
                       decompose=ehap.run(action="decompose"))
  savec(ehap.results)
}


#' augment a data table with expression.configs and expression.info
make.exp.search.decomp <- function(d) {
  result <- merge(d, expression.info, by="id", all=TRUE)
  result <- merge(result,
                  expression.configs[, c("config.label", "diffusion.label",
                                         expression.configs.strengthcols), with=FALSE],
                  by="config.label")
  setcolorder(result, c("config.label", "diffusion.label", "dataset",
                        "id", "name", "target", "target_name", "rank",
                        expression.configs.strengthcols))
  result[order(config.label, id, rank)]
}
if (!exists("expression.results")) {
  expression.results <- lapply(ehap.results, make.exp.search.decomp)
}


#' compute enrichment results with hypeR
make.hypeR <- function(genesets, target.genesets) {
  hyp.cols <- c("label", "pval", "fdr",
                "signature", "geneset", "overlap", "background")
  lapply(genesets, function(x) {
    hyp <- hypeR(x, target.genesets)
    result <- data.table(hyp$as.data.frame()[, hyp.cols])
    result[pval<0.05]
  })
}
if (!assignc("expression.hypeR")) {
  expression.hypeR <- make.hypeR(expression.genesets, targets.genesets)
  savec(expression.hypeR)
}

