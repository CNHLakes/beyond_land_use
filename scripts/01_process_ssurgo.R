library(raster)
library(sf)

gpkg_path <- path.expand("~/Documents/Science/Data/gSSURGO/")
ssurgo_path <- "data/gssurgo/"

source("scripts/utils.R")

r_list <- list.files(ssurgo_path, pattern = "^\\d*_\\d.tif$", 
                     include.dirs = TRUE, full.names = TRUE)

# llid <- 6874

r      <- raster(r_list[grep(llid, r_list)[1]])
bbox   <- st_bbox(r)
states <- get_states(bbox)

in_gpkg <- list.files(gpkg_path, pattern = paste0(states, ".gpkg"))


