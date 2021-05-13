#!/bin/bash -l
# script to run multiple crossmap rounds on the genesets data
#
# USAGE:
# just run the script from within the prep directory without arguments

cd ../data/hsapiens-genesets

CROSSMAP=../../crossmap

for SETTINGS in uniform
do
  echo "BUILDING SETTINGS=$SETTINGS"
  $CROSSMAP build --config config-hsapiens-genesets-$SETTINGS.yaml --logging INFO
  echo ""
done

