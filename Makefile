
.PHONY: data all

all: data figures data/dt.rds

data: data/ep.rds data/usgs/usgs.rds data/cdl/cdl.csv data/cdl/cdl_summary.csv data/gssurgo/gssurgo_key.csv

gssurgo: data/gssurgo/gssurgo.rds

cdl: data/cdl/cdl_summary.csv

usgs: data/usgs/usgs.rds

data/ep.rds: scripts/00_get_ep.R data/iws_lulc.rds data/county_lulc.rds
	Rscript $<

data/iws_lulc.rds: scripts/00_get_lulc.R
	Rscript $<

data/county_lulc.rds: scripts/00_get_lulc.R
	Rscript $<

data/usgs/usgs.rds: scripts/00_get_usgs.R
	Rscript $<

data/cdl/cdl.csv: scripts/00_get_cdl.R
	Rscript $< 'data/cdl/'
	
data/cdl/cdl_summary.csv: scripts/01_process_cdl.R data/cdl/cdl.csv data/cdl/cdl_key.csv
	Rscript $< 
	
data/cdl/cdl_key.csv: scripts/99_make_cdl_key.R
	Rscript $<
	
data/gssurgo/gssurgo_key.csv: scripts/99_make_ssurgo_key.R
	Rscript $<
	
data/gssurgo/gssurgo.csv: scripts/00_get_ssurgo.R
	Rscript $< 'data/gssurgo/'
	
data/gssurgo/gssurgo.rds: scripts/01_process_ssurgo.R data/gssurgo/gssurgo.csv
	Rscript $<

data/dt.rds: scripts/02_aggregate_predictors.R data/ep.rds data/iws_lulc.rds data/usgs/usgs.rds
	Rscript $<

figures: manuscript/figures.pdf

manuscript/figures.pdf: manuscript/figures.Rmd figures/01_county_extent-1.pdf
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	-pdftk manuscript/figures.pdf cat 2-end output manuscript/figures2.pdf
	-mv manuscript/figures2.pdf manuscript/figures.pdf
	
figures/01_county_extent-1.pdf: figures/01_county_extent.Rmd scripts/explore_lagos_ag.R
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"

tables: manuscript/tables.pdf

manuscript/tables.pdf: tables/01_predictors.pdf
	pdftk $^ cat output manuscript/tables.pdf
	
tables/01_predictors.pdf: tables/01_predictors.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
