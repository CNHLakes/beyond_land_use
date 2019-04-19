RAWLLIDS := data/llids.txt
VARLLIDS := $(shell cat ${RAWLLIDS})

.PHONY: data all

all: data figures tables data/dt.rds

data: data/ep.rds \
data/usgs/usgs.rds \
data/cdl/cdl.csv \
data/cdl/cdl_summary.csv \
data/gssurgo/gssurgo.rds \
data/census/census.rds \
data/macroag/tillage.gpkg \
data/macroag/crp.rds \
data/gis.gpkg \
data/llids.txt \
data/buffer_lulc.csv

gssurgo: data/gssurgo/gssurgo.rds

cdl: data/cdl/cdl_summary.csv

usgs: data/usgs/usgs.rds

data/gis.gpkg: scripts/00_get_gis.R
	Rscript $<

data/ep.rds: scripts/00_get_ep.R data/iws_lulc.rds data/county_lulc.rds
	Rscript $<
	
data/llids.txt: scripts/00_list_llids.R data/ep.rds
	Rscript $<

buffer_lulc: $(VARLLIDS)
	echo buffers pulled
	
data/buffer_lulc/%.csv: scripts/00_get_buffers.R
	Rscript $< $(basename $@)
	
data/buffer_lulc.csv: scripts/01_process_buffers.R
	Rscript $<

data/iws_lulc.rds: scripts/00_get_lulc.R
	Rscript $<

data/county_lulc.rds: scripts/00_get_lulc.R
	Rscript $<

data/usgs/usgs.rds: scripts/00_get_usgs.R data/ep.rds
	Rscript $<

data/cdl/cdl.csv: scripts/00_get_cdl.R
	Rscript $< 'data/cdl/'
	
data/cdl/cdl_summary.csv: scripts/01_process_cdl.R data/cdl/cdl.csv data/cdl/cdl_key.csv
	Rscript $< 
	
data/cdl/cdl_key.csv: scripts/99_make_cdl_key.R
	Rscript $<
	
data/gssurgo/gssurgo_key.csv: scripts/99_make_ssurgo_key.R
	Rscript $<
	
data/gssurgo/gssurgo.csv: scripts/00_get_ssurgo.R data/ep.rds
	Rscript $< 'data/gssurgo/'
	
data/gssurgo/gssurgo.rds: scripts/01_process_ssurgo.R data/gssurgo/gssurgo.csv
	Rscript $<

data/census/census.rds: scripts/00_get_census.R
	Rscript $<

data/macroag/tillage.gpkg: scripts/00_get_tillage.R
	Rscript $<

data/macroag/crp.rds: scripts/00_get_crp.R
	Rscript $<

data/dt.rds: scripts/02_aggregate_predictors.R data/ep.rds data/iws_lulc.rds data/usgs/usgs.rds data/gssurgo/gssurgo.rds data/buffer_lulc.csv
	Rscript $<
	
data/buffer_stats.csv: scripts/00_get_buffers.R
	Rscript $<

figures: manuscript/figures.pdf

manuscript/figures.pdf: manuscript/figures.Rmd figures/01_county_extent-1.pdf figures/02_hierarchical_demo-1.pdf figures/03_wetland_potential-1.pdf figures/04_nlcd-versus-cdl-1.pdf figures/05_cafos-1.pdf figures/06_lulc_buffer_demo-1.pdf
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	-pdftk manuscript/figures.pdf cat 2-end output manuscript/figures2.pdf
	-mv manuscript/figures2.pdf manuscript/figures.pdf
	
figures/01_county_extent-1.pdf: figures/01_county_extent.Rmd scripts/explore_lagos_ag.R
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	
figures/02_hierarchical_demo-1.pdf: figures/02_hierarchical_demo.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	
figures/03_wetland_potential-1.pdf: figures/03_wetland_potential.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"

figures/04_nlcd-versus-cdl-1.pdf: figures/04_nlcd-versus-cdl.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"

figures/05_cafos-1.pdf: figures/05_cafos.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"

figures/06_lulc_buffer_demo-1.pdf: figures/06_lulc_buffer_demo.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"

tables: manuscript/tables.pdf

manuscript/tables.pdf: tables/01_predictors.pdf
	pdftk $^ cat output manuscript/tables.pdf
	
tables/01_predictors.pdf: tables/01_predictors.Rmd data/dt.rds
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
