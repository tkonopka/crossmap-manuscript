# configuration variables


# ############################################################################
# libraries/packages

suppressMessages(library(data.table))
suppressMessages(library(igraph))
suppressMessages(library(R.utils))
suppressMessages(library(viridis))
library(parallel)
library(yaml)
suppressMessages(library(jsonlite))
library(curl)
library(shape)
suppressMessages(library(Rcssplot))
library(shrt)                            # github.com/tkonopka/shrt
library(batchFisher)                     # github.com/tkonopka/batchFisher
suppressMessages(library(umap))
library(beeswarm)
library(hypeR)


# ############################################################################
# paths to directories
# (this assumes the script is executed from vignettes/)

R.dir <- file.path("..", "R")
scripts.dir <- file.path("..", "scripts")
data.dir <- file.path("..", "data")
results.dir <- file.path("..", "results")


# ############################################################################
# custom functions from R directory

.rfiles <- c("kmers", "genes", "run", "dt_quantiles", "tools_diffusion",
             "tools_crossmap", "tools_ontology",
             "tauprecision", "tauperformance",
             "plot_scatter", "plot_chromosome", "plot_beeswarm",
             "plot_blob", "plot_general", "plot_graph", "plot_hist",
             "plot_schematic", "plot_benchmarks", "plot_perfbars",
             "plot_stackedbars", "plot_tauprecision",
             "plot_separate", "plot_qboxes", "plot_barplot",
             "plot_calibration", "plot_examples",
             "plot_training", "plot_trends", "plot_table")
for (.rfile in .rfiles) {
  source(file.path(R.dir, paste0(.rfile, ".R")))
}
rm(.rfile, .rfiles)




# ############################################################################
# constants, thesholds, paths, etc

# path to command-line crossmap program
crossmap.executable <- file.path("..", "crossmap")


# path to graphics styles
RcssDefaultStyle <- Rcss(c("Crossmap.Rcss", "Crossmap_diagrams.Rcss",
                          "Crossmap_diagnostics.Rcss"))

# labels to designate plot panels
panel.labels <- LETTERS
show.panel.labels <- TRUE

# cache directory (package shrt)
cachedir(file.path("..", "cache"))
