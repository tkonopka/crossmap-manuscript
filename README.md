# crossmap-manuscript

Analyses that accompany a manuscript describing the 
[crossmap](https://github.com/tkonopka/crossmap) software.

This repository describes analyses of large datasets that are not included
on github. A snapshot of data files - both raw and processed - is available at 
on zenodo.

The repository can be used in two distinct ways. It is possible to use the 
code to start with raw data files and perform a complete analysis, including 
data-intensive calculations, to produce summary visualizations. Alternatively,
it is possible to start with raw and processed data files, and only generate
visualization based on the preprocessed data. 


## Setup for a complete analysis

The repository holds code for creating knowledge-bases with crossmap, using
these knowledge-bases to process datasets, and visualize results. In order
to execute these steps, several set up steps are required. Apart from the setup,
the total running-time for these calculations can be several hours.


### crossmap software

The crossmap software should be installed in a location that is 
separate from this repository to avoid any potential file conflicts. 

An executable bash script called `crossmap` should be created at the repository
root to allow the repository to utilize the software. An example script: 

```
#!/bin/bash -l
python3 /path/to/crossmap/crossmap.py $@
```

The crossmap software will require access to a mongodb instance. See the 
[crossmap](https://github.com/tkonopka/crossmap) documentation.


### R packages

A list of required packages appears in file `vignettes/config.R`. Most packages
are available from CRAN while others can be installed through github.


### Data files

A set of data files must be provided into a `data` directory. The required 
data files include ontology files, gene annotations from several sources, and
other data. A snapshot of all the required files can be obtained from the 
`data` folder in the zenodo dataset.


### crossmap instances

Raw data must be transferred into crossmap instances. This can be performed
by executing scripts in the `prep` directory. The `README.md` in that directory
has additional explanation on each script.


### Vignettes

The `vignettes` folder contains two `Rmd` files and a number of other helper
files (R scripts, style sheets, etc.)

To compile the vignettes, navigate to the `vignettes` folder, start a new R 
session, and render the vignettes.

```
render("CrossmapFigures.Rmd")
render("CrossmapSupplementary.Rmd")
```

When these vignettes are executed for the first time, they perform 
compute-intensive calculations using the crossmap instances and custom R 
functions. The combined running time can reach several hours. Outputs from 
these calculations will appear in `results` and `cache` directories. The
rendered vignettes will themselves consist of pdf files in the `vignettes`
directory.

When the scripts are executed again, even starting from a clean R session,
the vignettes will utilize the cached data and regenerate pdf documents in 
minutes.


## Setup for an analysis using preprocessed data

It is possible to use the repository together with precomputed data files
and produce summary figures bypassing compute-intensive steps. The running
time for this approach should be around a minute.


### Raw and cached data files

A snapshot of all raw and data files is available in a zenodo repository. Copy
the content of `data`, `results`, and `cache` directories into the root of
the repository.

### Analysis using cached data files

Summary figures are generated via vignettes. Follow the instructions regarding
the installation of R packages and vignette rendering (above).

