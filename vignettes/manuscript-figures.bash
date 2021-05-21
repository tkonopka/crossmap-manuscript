#!/bin/bash -l


# directories with vignette outputs (pdfs)
PRIMARY=CrossmapFigures_files/figure-latex
SUPPLEMENTARY=CrossmapSupplementary_files/figure-latex
# directory to store output figures (png, tiff)
OUTFIGURES=figures

if [ ! -d "$OUTFIGUES" ]
then
    mkdir $OUTFIGURES
fi


# manually assembled figures
inkscape docs/cartoon.svg -e $OUTFIGURES/Fig-cartoon.png -d 600 --without-gui
inkscape docs/screenshot.svg -e $OUTFIGURES/Fig-screenshot.png -d 300 --without-gui
convert -flatten $OUTFIGURES/Fig-cartoon.png $OUTFIGURES/Fig-cartoon.tiff
convert -flatten $OUTFIGURES/Fig-screenshot.png $OUTFIGURES/Fig-screenshot.tiff


# primary figures 
for figname in decomposition translation diseases;
do
    gs -dNOPAUSE -r600x600 -q -sDEVICE=tiffscaled24 -dBATCH -sCompression=lzw \
       -sOutputFile=$OUTFIGURES/Fig-$figname.tiff $PRIMARY/fig.$figname-1.pdf
    gs -dNOPAUSE -r600x600 -q -sDEVICE=png16m -dBATCH \
       -sOutputFile=$OUTFIGURES/Fig-$figname.png $PRIMARY/fig.$figname-1.pdf
done


# supplementary figures
for figname in benchmarks lexical calibration;
do
    gs -dNOPAUSE -r600x600 -q -sDEVICE=tiffscaled24 -dBATCH -sCompression=lzw \
       -sOutputFile=$OUTFIGURES/Fig-$figname.tiff $SUPPLEMENTARY/fig.$figname-1.pdf
    gs -dNOPAUSE -r600x600 -q -sDEVICE=png16m -dBATCH \
       -sOutputFile=$OUTFIGURES/Fig-$figname.png $SUPPLEMENTARY/fig.$figname-1.pdf
done


