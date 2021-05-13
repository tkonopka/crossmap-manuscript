# sub analysis of Crossmap.Rmd
# evaluation of search and decomposition using genesets


# location on disk of geneset datasets
benchmarking.dir <- file.path(data.dir, "go-benchmarking")
benchmarking.file <- function(x) {
  file.path(benchmarking.dir, x)
}
go.data.file <- benchmarking.file("hsapiens-go-bp-midsizes.yaml.gz")
go.benchmark.template <- benchmarking.file("hsapiens-go-bp-benchmarking-SIZE.yaml.gz")
go.genes.txt <- benchmarking.file("hsapiens-go-bp.genes.txt")

# file path to dataset with go benchmarks
go.benchmark.file <- gsub("SIZE", go.benchmarks.size, go.benchmark.template)
genesets.config.template <-
  benchmarking.file("config-go-benchmarking-SETTINGS.yaml")


if (!assignc("benchmarks.configs")) {
  genesets.diffusion <- make.string.dicts(list(targets=c(0, 0.05, 0.1, 0.2)))
  benchmarks.configs <- make.configs.table(
    list(SETTINGS=c("uniform", "ic"),
         n=5,
         diffusion=names(genesets.diffusion)),
    label=TRUE, template=genesets.config.template,
    prefix="config")
  benchmarks.configs$diffusion <-
    as.character(genesets.diffusion[benchmarks.configs$diffusion])
  savec(benchmarks.configs)
}


if (!assignc("go.data")) {
  make.go.data <- function() {
    result <- read_yaml(go.data.file)
    # reformat into R-friendly lists
    for (x in names(result)) {
      temp <- result[[x]]
      temp$genes <- temp$data$symbols
      temp$data <- NULL
      xparts <- strsplit(x, "_")[[1]]
      temp$id <- xparts[2]
      temp$branch <- xparts[1]
      result[[x]] <- temp
    }
    result       
  }
  go.data <- make.go.data()
  savec(go.data)
}
if (!exists(go.genes.txt)) {
  fwrite(data.table(gene=unique(unlist(lapply(go.data,
                                              function(x) { x$genes})))),
         file=go.genes.txt, col.names=FALSE)
}


if (!assignc("go.sets")) {
  go.sets <- lapply(go.data, function(x) { x$genes })
  names(go.sets) <- sapply(go.data, function(x) { x$id })
  savec(go.sets)
}
if (!assignc("go.intsets")) {
  go.universe <- sort(unique(unlist(go.sets)))
  go.intsets <- lapply(go.sets, function(x) {
    which(go.universe %in% x)
  })
  savec(go.intsets)
}
n.go.genes <- length(unique(unlist(go.intsets)))



#' create a dataset with randomly selected genes from two or three processes
#'
#' @param geneset list of containing $id, $genes
#' @param n integer, number of items (for each
#' @param min.size integer, min number of genes in geneset to avoid using sets
#' that contain only a couple of genes
#' @param components integer vector, number of genesets to include in each pool
#' @param coverage numeric vector, proportion of geneset to include from each
#' set
#' @param snr numeric vector, signal-to-noise ratio, proportion of genes
#' in benchmark set from genesets vs from overall pool
#' @param prefix character, used to define ids for the benchmark items
#'
#' @return list with crossmap-compatible dataset
make.gene.benchmark.dataset <- function(genesets, n=1000,
                                        min.size=10,
                                        components=c(1,2,3,4),
                                        coverage=c(0.25, 0.5, 0.75, 1),
                                        signal=c(0.25, 0.5, 0.75, 1),
                                        prefix="benchmark") {
  
  all.genes <- unique(unlist(lapply(genesets,
                                    function(x) { x$genes })))
  # filter genesets, select only those that have a minimal size
  genesets <- genesets[sapply(genesets,
                              function(x) { length(x$genes)>=min.size })]

  # shortcuts into content of the genesets list
  genesets.ids <- sapply(genesets, function(x) { x$id })
  genesets.genes <- lapply(genesets, function(x) { x$genes })
  genesets.titles <- sapply(genesets, function(x) { x$title })
  
  configs <- expand.grid(list(rep=seq_len(n),
                              components=components,
                              coverage=coverage,
                              signal=signal))
  configs <- split(configs, 1:nrow(configs))
  result <- lapply(configs, function(config) {
    # select a random set of components    
    selection <- sample(names(genesets), config$components, replace=FALSE)
    selection.ids <- setNames(genesets.ids[selection], NULL)
    # select a subset of the genes from each componet
    sample.genes <- list()
    for (id in selection) {
      id.genes <- genesets.genes[[id]]
      id.count <- max(1, floor(config$coverage*length(id.genes)))
      sample.genes[[id]] <- sample(id.genes, id.count, replace=FALSE)
    }
    sample.counts <- setNames(sapply(sample.genes, length), NULL)
    sample.genes <- setNames(unlist(sample.genes), NULL)
    sample.sources <- setNames(selection.ids, NULL)
    sample.titles <- setNames(genesets.titles[selection], NULL)
    sig <- config$signal
    if (sig<1) {
      num.noise <- ceiling(((1-sig)/sig)*length(sample.genes))
      noise.genes <- sample(all.genes, num.noise, replace=FALSE)
      noise.genes <- setdiff(noise.genes, sample.genes)
      sample.genes <- c(sample.genes, noise.genes)
      sample.counts <- c(sample.counts, length(noise.genes))
      sample.sources <- c(sample.sources, "random")
      sample.titles <- c(sample.titles, "random")
    }
    metadata <- list(sources=sample.sources, titles=sample.titles,
                     counts=sample.counts,
                     coverage=config$coverage, signal=sig)
    list(data=sample.genes, metadata=metadata)
  })
  names(result) <- paste0(prefix, ":", seq_along(result))
  result
}


if (!assignc("benchmarks.sets") |
  !assignc("benchmarks.expected") |
  !assignc("benchmarks.summary")) {
  # the benchmarks.sets and .expected objects rely on knowledge from the raw
  # benchmark file, so load that up here  
  # load up synthetic genesets (there are composed of bits from one, two,
  # or more GO sets)
  if (!file.exists(go.benchmark.file)) {
    gene.benchmark.dataset <- make.gene.benchmark.dataset(go.data, go.benchmarks.size)
    write_yaml(gene.benchmark.dataset, gsub(".gz", "", go.benchmark.file))
    gzip(gsub(".gz", "", go.benchmark.file))
  } else {
    gene.benchmark.dataset <- read_yaml(go.benchmark.file)
  }
  
  # extract only the genes that make up the benchmark sets
  benchmarks.sets <- lapply(gene.benchmark.dataset, function(x) { x$data })
  savec(benchmarks.sets)
  
  # create a summary from gene.benchmark.dataset but in a data.table format
  make.benchmarks.expected <- function() {
    result <- vector("list", length(gene.benchmark.dataset))
    names(result) <- names(gene.benchmark.dataset)
    for (b in names(gene.benchmark.dataset)) {
      xm <- gene.benchmark.dataset[[b]]$metadata
      result[[b]] <- data.table(id=b,
                                target=xm$sources,
                                counts=xm$counts)
    }
    rbindlist(result)
  }
  benchmarks.expected <- make.benchmarks.expected()
  savec(benchmarks.expected)
  
  make.benchmarks.summary <- function() {
    result <- vector("list", length(gene.benchmark.dataset))
    names(result) <- names(gene.benchmark.dataset)
    for (b in names(gene.benchmark.dataset)) {
      xm <- gene.benchmark.dataset[[b]]$metadata
      num.components <- length(xm$sources)
      num.random <- 0
      if ("random" %in% xm$sources) {
        num.components <- num.components-1
        num.random <- xm$counts[length(xm$counts)]
      }      
      result[[b]] <- data.table(
        id=b,
        coverage=xm$coverage,
        components=length(setdiff(xm$sources, "random")),
        counts_components=paste(xm$counts[1:num.components], collapse=","),
        total_components=sum(xm$counts[1:num.components]),
        total_random=num.random,
        signal=xm$signal)
    }
    rbindlist(result)
  }
  benchmarks.summary <- make.benchmarks.summary()
  savec(benchmarks.summary)
  fwrite(benchmarks.summary,
         file=file.path(results.dir, "benchmarks.summary.tsv"), sep="\t")
}


if (!assignc("precomputed.fisher")) {
  # compute fisher p-values for configurations that are bound to occur
  # several times
  make.precomputed <- function(a.sets, b.sets, or=c(20, 30)) {
    # characterize the sets
    universe <- unique(c(unique(unlist(a.sets)), unique(unlist(b.sets))))
    u.size <- length(universe)
    get.freq.sizes <- function(sets) {
      sizes <- sapply(sets, length)
      sort(unique(sizes[duplicated(sizes)]))
    }
    a.freq <- get.freq.sizes(a.sets)
    b.freq <- get.freq.sizes(b.sets)
    # compute low odds-ratio results for all combinations
    part1 <- precompute_fisher(u.size, a_sizes=a.freq, b_sizes=b.freq,
                               max_or=or[1])
    # compute higher odds-ratio result for smaller sizes
    # this is an ad-hoc method
    # to provide more precomputed results for small sets
    # (smaller sets are more common)
    part2 <- precompute_fisher(u.size,
                               a_sizes=head(a.freq, length(a.freq)*0.6),
                               b_sizes=head(b.freq, length(b.freq)*0.6),
                               max_or=or[2])
    result <- rbind(part1, part2)
    result$p.value <- signif(result$p.value, 8)
    result$odds.ratio <- signif(result$odds.ratio, 8)
    unique(result[order(count_11, count_10, count_01)])
  }
  precomputed.fisher <- make.precomputed(benchmarks.sets, go.sets,
                                         or=c(20, 60))
  savec(precomputed.fisher)
  gc()
}


if (!assignc("benchmarks.fisher.results")) {
  #' compute fisher tests between benchmark sets and go sets
  #' @param p numeric, p-value threshold 
  #' @param cores integer, number of compute cores to use
  #' @return tables with top associations (p-value lower than p threshold)
  make.fisher.results <- function(p=0.05, cores=detectCores()) {
    # convert go sets into lists mapping ID to integers
    all.genes <- sort(unique(unlist(go.sets)))
    n.go.genes <- length(all.genes)
    gene.2.int <- function(x) { which(all.genes %in% x) }
    int.go <- lapply(go.sets, gene.2.int)
    int.benchmarks <- lapply(benchmarks.sets, gene.2.int)
    int.sets <- c(int.go, int.benchmarks)
    rm(int.go, int.benchmarks)
    result <- mclapply(names(benchmarks.sets), function(x) {
      if (endsWith(x, "00")) {
        print(paste(date(), x))
      }
      tabs <- batch_contingency(rep(x, length(go.sets)),
                                names(go.sets),
                                n.go.genes,
                                sets=int.sets)
      setnames(tabs, c("a", "b"), c("id", "target"))
      x.result <- batch_fisher(tabs, precomputed.fisher)
      x.result[, c("count_11", "count_10", "count_01", "count_00")] = NULL
      x.result[p.value<p][order(p.value)]
    }, mc.cores=cores)
    rbindlist(result)
  }
  benchmarks.fisher.results <- make.fisher.results()
  benchmarks.fisher.results$config.label <- "p0.05"
  benchmarks.fisher.results$method <- "fisher"
  savec(benchmarks.fisher.results)
}


if (!assignc("benchmarks.search.results")) {
  benchmarks.search.results <-
    configsrun.search(benchmarks.configs, go.benchmark.file, cores=detectCores())
  benchmarks.search.results$method <- "search"
  benchmarks.search.results$target <- gsub("BP_|MF_|CC_", "",
                                           benchmarks.search.results$target)
  savec(benchmarks.search.results)
}
if (!assignc("benchmarks.decomp.results")) {
  benchmarks.decomp.results <-
    configsrun.decompose(benchmarks.configs,
                         go.benchmark.file, cores=detectCores())
  benchmarks.decomp.results$method <- "decomp"
  benchmarks.decomp.results$target <- gsub("BP_|MF_|CC_", "",
                                           benchmarks.decomp.results$target)
  savec(benchmarks.decomp.results)
}


if (!assignc("benchmarks.performance")) {
  make.benchmarks.performance.part <- function(predictions, tau) {
    sort.by <- "p.value"
    if ("rank" %in% colnames(predictions)) {
      sort.by <- "rank"
    }
    configs.tauprecision(predictions, benchmarks.expected,
                         tau=tau, sets=go.intsets, sort.by=sort.by)
  }
  make.benchmarks.performance <- function() {
    make.part <- make.benchmarks.performance.part
    tau <- 0
    rbindlist(list(
      make.part(benchmarks.fisher.results, tau=tau),
      make.part(benchmarks.search.results, tau=tau),
      make.part(benchmarks.decomp.results, tau=tau)
    ))
  }
  benchmarks.performance <- make.benchmarks.performance()
  savec(benchmarks.performance)
}


#' create a list of lists suitable for plot_boxtrends
#'
#' (This function is used in the vignette to summarize performance on
#' the genesets benchmarks before plotting)
#'
#' @param tau numeric, value of tau
#' @param num.components integer, extract for benchmarks with a set
#' number of components
#' @param methods data table with $method and $config.labels
#'
#' @return list of lists, first level labeled by signal purity,
#' second level labeled by methods
signal.benchmarks.performance <- function(tau=0, num.components=1,
                                          methods=NULL) {
  # find the benchmark sets that fit the criteria
  benchmarks <- benchmarks.summary[components==num.components]
  # simplify the performance table to the relevant sets and methods
  mc <- c("method", "config.label", "series")
  methods <- data.table(as.data.frame(methods)[, mc, drop=FALSE])
  .tau <- tau
  perf <- merge(benchmarks.performance[tau==.tau], methods,
                by=c("method", "config.label"))
  perf <- merge(perf, benchmarks, by="id")
  # construct the output object
  result <- perf[, list(tauprecision=list(tauprecision)),
                   by=c(mc, "signal")]
  result
}

