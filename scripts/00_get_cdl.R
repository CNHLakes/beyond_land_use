cmdargs <- commandArgs(trailingOnly = TRUE)
cdl_path <- cmdargs[1]
# cdl_path <- "data/cdl/"

library(LAGOSNE)
library(LAGOSextra)
library(sf)
library(cdlTools)
library(raster)
library(dplyr)
library(concaveman)

source("scripts/utils.R")

lg <- lagosne_load("1.087.1")
ep     <- readRDS("data/ep.rds")
ep_bbox <- st_bbox(concaveman(coordinatize(ep)))
# mapview::mapview(LAGOSNE::coordinatize(ep))
ep     <- distinct(ep, lagoslakeid)

# ep <- filter(ep, lagoslakeid %in% c(23670, 5724))
for(i in seq_len(length(ep$lagoslakeid))){
  llid <- ep$lagoslakeid[i]
  print(llid)
  # i <- 1
  # llid <- 23670
  boundary_iws <- get_iws(llid)
  bbox         <- get_bbox(boundary_iws)
  states       <- get_states(bbox)

  # pull each cdl

  # 2008 might by minimum year with all states?
  left_join(ep, select(lg$locus, lagoslakeid, state_zoneid)) %>%
    left_join(select(lg$state, state_zoneid, state)) %>%
    mutate(state = as.character(state)) %>%
    distinct(state) %>%
    sapply(fips) # for manual downloading

  cdl <- cdlTools::getCDL(x = states, year = 2008,
                           ssl.verifypeer = FALSE,
                           location = cdl_path)

  # mosaic cdls
  mosaic_path <- paste0(cdl_path, paste0(states, collapse = "_"), ".tif")
  if(!file.exists(mosaic_path)){
    system(paste0("gdal_merge.py -o " , mosaic_path, " -n 0.0 ",
                  paste0(
                    unlist(lapply(cdl, function(x) get_raster_name(x, cdl_path))), ".tif", collapse = " ")))
  }

  iws_raster_path <- paste0(cdl_path, paste0(llid, ".tif"))

  if(!file.exists(iws_raster_path)){
    system(paste0("gdal_translate -projwin ",
                  paste0(
                    as.vector(bbox)[c(1, 4, 3, 2)], collapse = " "),
                  " -of GTiff ",
                  mosaic_path, " ",
                  iws_raster_path))

    iws_raster <- raster(iws_raster_path)
    iws_raster <- mask(iws_raster, boundary_iws)
    writeRaster(iws_raster, iws_raster_path, overwrite = TRUE)
  }
}

write.csv(ep, file.path(cdl_path, "cdl.csv"), row.names = FALSE)
