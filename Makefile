include config.mk

test_config:
	ls $(gssurgo_path)
	ls $(cdl_path)
	ls $(usgs_path)
	@echo $(STATES)

.PHONY: data all

all: data

data: data/ep.rds data/usgs/usgs.rds data/cdl/cdl.csv data/cdl/cdl_summary.csv

data/ep.rds: scripts/00_get_ep.R data/iws_lulc.rds data/county_lulc.rds
	Rscript $<

data/iws_lulc.rds: scripts/00_get_lulc.R
	Rscript $<

data/county_lulc.rds: scripts/00_get_lulc.R
	Rscript $<

data/usgs/usgs.rds: scripts/00_get_usgs.R data/ep.rds
	Rscript $<

data/cdl/cdl.csv: scripts/00_get_cdl.R
	Rscript $< 'data/cdl/'
	
data/cdl/cdl_summary.csv: scripts/01_process_cdl.R data/cdl/cdl.csv
	Rscript $< 
	