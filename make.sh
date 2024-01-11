#!/bin/bash

cabal run
sed 's/haskell \(ignore\|top\)/haskell/g' src/icfp.md > tmp/icfp.md
pandoc tmp/icfp.md -o tmp/icfp.tex
cd template
pdflatex icfp-template.tex
mv icfp-template.pdf ../icfp.pdf
cd ..

