[![Paper DOI](https://img.shields.io/badge/Paper-DOI-blue.svg)](https://doi.org) [![Code DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3754916.svg)](https://doi.org/10.5281/zenodo.3754916) [![Docker Build](https://img.shields.io/badge/Docker%20Image-jsta/beyond_land_use-green.svg)](https://cloud.docker.com/repository/docker/jsta/beyond_land_use)

Code and data
for:

**Stachelek et al. In press**. Granular measures of agricultural land-use influence lake nitrogen and phosphorus differently at macroscales. _Ecological Applications_.

### Products

Figures: [manuscript/figures.pdf](manuscript/figures.pdf)

Appendix: [manuscript/appendix.pdf](manuscript/figures.pdf)

### Contents

<!--- tree -I '*.pdf|NLCD|*.png|*.tex|*.tif*|RAW|*.html|*.Rproj|EXTRACTIONS|backup*|old|*.jpg|*.csv|*.gpkg|*.js|cafos|fe|re|reilly*|rosm.cache|*.rds|*.xls|*.xlsx|re_40|*.sql|fe_nolulc|example*|temp.py|diagram|cdlTools.*|USGS.R|rnassqs.R|riparian_lulc.R|explore_lagos_ag.R' -->

<pre>
.
├── data
│   ├── buffer_lulc
│   ├── cdl
│   │   └── Makefile
│   ├── census
│   ├── gssurgo
│   ├── llids.txt
│   ├── macroag
│   ├── mapbox
│   ├── mcmc
│   ├── nhd
│   └── usgs
├── figures
│   ├── 01_county_extent.Rmd
│   ├── 02_hierarchical_demo.Rmd
│   ├── 03_wetland_potential.Rmd
│   ├── 04_nlcd-versus-cdl.Rmd
│   ├── 05_cafos.Rmd
│   ├── 06_lulc_buffer_demo.Rmd
│   ├── 07_model-selection_nolulc.Rmd
│   ├── 07_model-selection.Rmd
│   ├── 08_exploratory_dotplot.Rmd
│   ├── 09_stream_buffer.Rmd
│   ├── 11_map.Rmd
│   └── Makefile
├── Makefile
├── manuscript
│   ├── appendix.Rmd
│   └── figures.Rmd
├── README.md
├── scripts
│   ├── 00_get_buffers.R
│   ├── 00_get_cdl.R
│   ├── 00_get_census.R
│   ├── 00_get_crp.R
│   ├── 00_get_ep.R
│   ├── 00_get_gis.R
│   ├── 00_get_lulc.R
│   ├── 00_get_ssurgo.R
│   ├── 00_get_tillage.R
│   ├── 00_get_usgs.R
│   ├── 00_list_llids.R
│   ├── 01_process_buffers.R
│   ├── 01_process_cdl.R
│   ├── 01_process_ssurgo.R
│   ├── 02_aggregate_predictors.R
│   ├── 03_model_nolulc.R
│   ├── 03_model.R
│   ├── 99_make_cdl_key.R
│   ├── 99_make_predictor_key.R
│   ├── 99_make_ssurgo_key.R
│   ├── 99_utils.R
│   ├── construct_sql.R
│   └── epi_nutr.R
└── tables
    ├── 01_predictors.Rmd
    ├── 02_cdl_key.Rmd
    ├── 03_model_summary.Rmd
    ├── 04_hu4s.Rmd
    └── Makefile
</pre>

### Dependencies

Primary dependencies include the `dplyr`, `sf`, `LAGOSNE`, `nhdR`, and `brms` R packages. See [scripts/99_utils.R](scripts/99_utils.R) for a full list.

### Reproducibility

  * Requires linking to LAGOSNE-GIS as a Docker volume. Get data with the `LAGOSNEgis` package [![DOI](https://zenodo.org/badge/106293356.svg)](https://zenodo.org/badge/latestdoi/106293356)

```
docker pull jsta/beyond_land_use
docker run -e PASSWORD=<PASSWORD> --rm -v ~/.local/share/LAGOS-GIS:/root/.local/share/LAGOS-GIS jsta/stachelek_soranno_2019
docker ps # note container "code name"
docker exec -ti <NAME> /bin/bash
make all
```
