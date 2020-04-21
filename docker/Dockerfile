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

RUN cd beyond_land_use && git pull

COPY data/mcmc/re_brms.rds /~/beyond_land_use/data/mcmc/