# prep
scripts to build crossmap instances from data files. 


## setup

The scripts assume that the repository root contains:
 - an executable `crossmap` that runs the crossmap.py program using python.

The scripts assume that the `data` folder contains:
 - folder `disease` with prepared data files and a configuration file 
   `config-disease-ic.yaml`
 - folder `go-benchmarking` with prepared data files and configuration
   files `config-go-benchmarking-uniform.yaml` and 
   `config-go-benchmarking-ic.yaml`.
 - folder `mp-translation` with prepared data files and a configuration file
   `config-mp-translation-parents.yaml`

The scripts assume that the `crossmap` instance will have access to a 
mongodb instance. The data folder contains a docker-compose file that launches
an appropriate database.


## execution

The scripts should be launched on the command line from within the `prep`
directory, without any arguments. 

```
./build-go-benchmarking.bash
./build-hsapiens-genesets.bash
./build-mp-translation.bash
./build-disease.bash
```
