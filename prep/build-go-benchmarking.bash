#!/bin/bash -l
# script to build crossmap instances for a benchmarking study with genesets
#
# USAGE:
# just run the script from within the prep directory without arguments

cd ../data/go-benchmarking

CROSSMAP=../../crossmap

for SETTINGS in uniform ic
do
  echo "BUILDING SETTINGS=$SETTINGS"
  $CROSSMAP build --config config-go-benchmarking-$SETTINGS.yaml --logging INFO
  echo ""
done

