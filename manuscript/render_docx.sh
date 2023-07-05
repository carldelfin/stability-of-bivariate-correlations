#!/bin/bash

# render rmarkdown
R -e 'rmarkdown::render("manuscript.Rmd")'
R -e 'rmarkdown::render("supplemental_material.Rmd")'

# convert to .docx
pandoc manuscript.tex --bibliography=bibliography.bib --reference-doc=template.docx -o manuscript.docx
pandoc supplemental_material.tex --bibliography=bibliography.bib --reference-doc=template.docx -o supplemental_material.docx

# clean up
rm manuscript.pdf
rm manuscript.tex
rm supplemental_material.pdf
rm supplemental_material.tex
rm -rf manuscript_files