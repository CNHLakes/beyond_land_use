manuscript.pdf: manuscript.Rmd \
pinp.cls \
lagosag.bib \
jsta.bst
	Rscript -e "rmarkdown::render('manuscript.Rmd')"
	pdftk manuscript.pdf appendix.pdf cat output manuscript2.pdf
	mv manuscript2.pdf manuscript.pdf
	