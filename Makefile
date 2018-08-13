include config.mk

test_config:
	ls $(gssurgo_path)
	ls $(cdl_path)
	ls $(usgs_path)
	@echo $(STATES)

.PHONY: data all

all: data

data: data/ep.rds data/usgs.rds

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