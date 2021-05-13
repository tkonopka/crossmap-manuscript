#!/bin/bash -l
# script to build crossmap instances using disease data
#
# USAGE:
# just run the script from within the prep directory without arguments

cd ../data/disease

CROSSMAP=../../crossmap

for SETTINGS in ic
do
  echo "BUILDING SETTINGS=$SETTINGS"
  $CROSSMAP build --config config-disease-$SETTINGS.yaml --logging INFO
  echo ""
done

