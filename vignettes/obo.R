# sub analysis of Crossmap.Rmd
# loading of ontology definitions


obo.dir <- file.path(data.dir, "obo")

# load some relevant ontology
if (!assignc("onto")) {
  onto <- read.ontology.data(c("mp", "hp", "go"), obo.dir)
  onto$graph <- igraph::graph_from_edgelist(as.matrix(onto$parents))
  savec(onto)
}

