# quasi-generic helper functions Crossmap.Rmd


#' read information about an ontology from disk files
#'
#' @param ontology.name character, e.g. mp, hp, go
#' @param dir character, directory path holding mp-ancestors.tsv.gz and
#' mp-names.tsv.gz
#'
#' @return list with ontology name and ancestors
read.ontology.data <- function(ontology.names, dir=".") {
  onto.prefix <- file.path(dir, ontology.names)
  names <- rbindlist(lapply(paste0(onto.prefix, "-names.tsv.gz"), fread))
  ancestors <- rbindlist(lapply(paste0(onto.prefix, "-ancestors.tsv.gz"), fread))
  parents <- rbindlist(lapply(paste0(onto.prefix, "-parents.tsv.gz"), fread))
  ancestors <- setNames(strsplit(ancestors$ancestors, ";"), ancestors$id)
  list(names=names, parents=parents, ancestors=ancestors)
}


#' collect data on an ontology graph from a crossmap data yaml file
#'
#' @param data.yaml path to data file
#' @param layout_fun function from igraph, acting on a graph and returning a
#' layout
#'
#' @return list with various tables pertaining to the heartbeat dataset
make.ontology.graph <- function(data.yaml, layout_fun=layout_with_lgl) {
  
  data <- yaml.load_file(data.yaml)
  
  # helpers acting on the data object from yaml
  data.ids <- function() {
    names(data)
  }
  data.names <- function() {
    sapply(data, function(z) { z$data$name })
  }
  data.parents <- function() {
    sapply(data, function(z) { paste(z$metadata$parents, collapse=",") })
  }
  
  # parse the raw data into nodes and edges
  result <- data.table(id=data.ids(),
                       name=data.names(),
                       parents=data.parents())
  result <- result[, list(parent=unlist(strsplit(parents, ","))),
                     by=c("id", "name")]
  
  # create graphs
  nodes <- unique(result[, c("id", "name")])
  edges <- unique(result[, c("id", "parent")])
  setnames(edges, c("id", "parent"), c("id", "target"))
  edges$target[!edges$target %in% edges$id] =NA
  edges <- edges[!is.na(target)]
  set.seed(nrow(edges)*123)
  graph <- igraph::graph_from_edgelist(as.matrix(edges))
  graph.layout <- layout_fun(graph)
  rownames(graph.layout) <- vertex_attr(graph)$name
  # create base output object
  result <- list(nodes = nodes,
                 edges = edges,
                 graph = graph,
                 layout = graph.layout)
  result
}


#' modify a data table by adding column with ontology names
#'
#' @param x data table
#' @param id.columns character, all columns that require annotating with
#' a name
#'
#' @return data table with augmented columns
add.ontology.names <- function(x,
                               id.columns=c("id", "target", "expected")) {
  result <- copy(x)
  # handle "id" specifically because underscore
  if ("id" %in% colnames(x) & !"name" %in% colnames(x)) {
    result <- merge(result, copy(onto$names), by="id", all.x=TRUE)
  }
  for (z in setdiff(id.columns, "id")) {
    z.name <- paste0(z, "_name")
    if (z %in% colnames(x) & !z.name %in% colnames(x)) {
      temp <- copy(onto$names)
      setnames(temp, c("id", "name"), c(z, z.name))
      result <- merge(result, temp, by=z, all.x=TRUE)
    }
  }
  result
}


#' assess quality of a mapping onto ontology targets
#'
#' @param observed data table, must contain columns id, target, rank
#' @param expected data table, must contain columns id, expected
#' @param onto list with ontology data, including component $ancestors.
#' This is used to score imperfect matches
#' @param by character vector, column in observed that characterize distinct
#' result sets
#'
#' @return list with tables, including $summary and $details
evaluate.ontology.mapping <- function(observed, expected, onto,
                                      by=c(), cores=NULL) {
  ontograph <- onto$graph
  if (is.null(cores)) {
    cores <- detectCores()
  }
  id.names <- copy(onto$names)
  expected.names <- copy(onto$names)
  setnames(expected.names, c("id", "name"), c("expected", "expected_name"))
  target.names <- copy(onto$names)
  setnames(target.names, c("id", "name"), c("target", "target_name"))
  # join up the results and expected values
  # evaluate precision of top hit and all top hits
  temp <- merge(observed, expected, by="id", allow.cartesian=TRUE)
  # precompute relevant path distances between expected and output nodes
  te.pairs <- unique(temp[, c("target", "expected")])
  te.list <- split(te.pairs, te.pairs$target)
  te.pairs <- rbindlist(mclapply(te.list, function(x) {
    from <- x$target[1]
    to <- x$expected
    x$path <- igraph::distances(ontograph, v=from, to=unique(to))[1, to]
    x
  }, mc.cores=cores))
  temp <- merge(temp, te.pairs, by=c("target", "expected"))
  
  result <-
    temp[, list(target=target[rank==1],
                target_N=paste(unique(target[order(rank)]), collapse=";"),
                precision=as.integer(target[rank==1] == expected),
                precision_bestN=as.integer(any(target == expected)),
                pathlen=as.integer(path[rank==1]),
                pathlen_bestN=as.integer(min(path)),
                pathlen_sumN=sum(path)),
           by=c("id", "expected", by)]
  
  # augment evaluation with names for the output terms
  result <- merge(result, id.names, by="id")
  result <- merge(result, expected.names, by="expected")
  result <- merge(result, target.names, by="target")
  result[order(id), c(by, "id", "name",
                      "expected", "expected_name",
                      "target", "target_name", "target_N",
                      "precision", "precision_bestN",
                      "pathlen", "pathlen_bestN", "pathlen_sumN"),
         with=FALSE]
}

