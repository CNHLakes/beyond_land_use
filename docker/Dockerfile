FROM rocker/geospatial:latest

LABEL maintainer="stachel2@msu.edu"

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    python3-pip \
    python3

RUN pip3 install gssurgo

RUN Rscript -e "install.packages(c('devtools','knitr','rmarkdown','RCurl'), repos = 'https://cran.rstudio.com')"

RUN Rscript -e "devtools::install_github(c('mikejohnson51/HydroData', 'cont-limno/LAGOSNEgis'))"

RUN Rscript -e "install.packages(c('dplyr', 'cowplot', 'sf', 'ggplot2'), repos = 'https://cran.rstudio.com')"

RUN Rscript -e "install.packages(c('kableExtra', 'lwgeom'), repos = 'https://cran.rstudio.com')"

WORKDIR ~/

RUN git clone https://github.com/CNHlakes/beyond_land_use.git

RUN cd beyond_land_use

RUN Rscript -e "remotes::install_cran('brms', force = FALSE)"

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    p7zip-full

RUN Rscript -e "install.packages(c('nhdR'), repos = 'https://cran.rstudio.com')"

RUN Rscript -e "install.packages(c('FedData', 'assertr', 'cdlTools', 'unpivotr'), repos = 'https://cran.rstudio.com')"

RUN Rscript -e "install.packages(c('snakecase', 'tidybayes'), repos = 'https://cran.rstudio.com')"

RUN Rscript -e "install.packages(c('gghighlight'), repos = 'https://cran.rstudio.com')"

RUN Rscript -e "devtools::install_github('cont-limno/LAGOSNE', ref = 'v1.087.1')"

RUN Rscript -e "LAGOSNE::lagosne_get('1.087.1', dest_folder = LAGOSNE:::lagos_path())"

RUN Rscript -e "install.packages(c('ggsn'), repos = 'https://cran.rstudio.com')"

RUN tlmgr install xcolor mdwtools multirow wrapfig colortbl tabu varwidth threeparttable threeparttablex environ trimspaces ulem makecell setspace psnfss

RUN apt-get install -y --no-install-recommends \
  pdftk

RUN Rscript -e "dir.create(rappdirs::user_data_dir(), recursive = TRUE)"

RUN Rscript -e "devtools::install_github('cont-limno/LAGOSNEgis')"

ENV LAGOSNEGIS_PATH=/root/.local/share/LAGOS-GIS/LAGOS_NE_GIS_Data_v1.0.gdb

RUN Rscript -e "install.packages(c('ggplotify', 'mgcViz'), repos = 'https://cran.rstudio.com')"

RUN Rscript -e "install.packages('rnaturalearthhires', repos = 'http://packages.ropensci.org', type = 'source')"

RUN Rscript -e "install.packages(c('rmapshaper', 'spind'), repos = 'https://cran.rstudio.com')"

# uncomment to copy-in component models
# COPY data/mcmc/re/tn_* data/mcmc/re/tp_* /~/beyond_land_use/data/mcmc/re/

# comment to avoid copy-in of model results list
COPY \
  data/mcmc/re_brms.rds \
  data/mcmc/fe_brms_nolulc.rds \
  /~/beyond_land_use/data/mcmc/

# comment to rebuild model results list
RUN touch /~/beyond_land_use/data/mcmc/re_brms.rds

RUN touch /~/beyond_land_use/data/mcmc/fe_brms_nolulc.rds

RUN cd beyond_land_use && git pull
