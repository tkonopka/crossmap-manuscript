#!/bin/bash -l
# script to run multiple crossmap rounds on the mp data
#
# USAGE:
# just run the script from within the prep directory without arguments

cd ../data/mp-translation

CROSSMAP=../../crossmap

for TARGETS in parents
do
    echo "BUILDING TARGETS=$TARGETS"
    $CROSSMAP build --config config-mp-translation-$TARGETS.yaml --logging INFO    
    $CROSSMAP add --config config-mp-translation-$TARGETS.yaml --logging INFO \
              --dataset manual --data hpmp-manual.yaml
    echo ""    
done

