all: rdss

rdss: data/ep.rds data/iws_lulc.rds data/county_lulc.rds

data/ep.rds: scripts/epi_nutr.R
	Rscript $<
	
data/iws_lulc.rds: scripts/epi_nutr.R
	Rscript $<

data/county_lulc: scripts/epi_nutr.R
	Rscript $<
	