#!/bin/bash

cabal run
perl -pi -w -e 's/<svg\s+(.*?)>/<svg $1 shape-rendering="crispEdges">/' figures/*svg

for file in figures/*.svg; do
  inkscape --file=$file --without-gui --export-pdf="${file%.svg}.pdf"
done

sed 's/haskell \(ignore\|top\)/haskell/g' src/icfp.md > tmp/icfp.md
pandoc --mathjax --citeproc tmp/icfp.md -o tmp/icfp.tex
cd template
pdflatex icfp-template.tex
mv icfp-template.pdf ../icfp.pdf
cd ..



