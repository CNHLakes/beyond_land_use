source("scripts/99_utils.R")

ep       <- readRDS("data/ep.rds")
in_path  <- path.expand("~/Documents/Science/Data/gssurgo_data/tifs/")
out_path <- path.expand("~/Documents/Science/Data/gssurgo_data/aois/")

existing_rasters <- list.files(out_path, pattern = "\\d*_\\d.tif", 
                               include.dirs = TRUE, full.names = TRUE)
existing_llids <- stringr::str_extract(existing_rasters, "(\\d*)(?=_\\d.tif)")

# unlink(existing_rasters[!(existing_llids %in% ep$lagoslakeid)])
ep <- dplyr::filter(ep, !(lagoslakeid %in% as.numeric(existing_llids)))

# i <- 1
# llid <- 6810
# llid <- 2057
# llid <- 6844
# llid <- 2036
# i <- which(ep$lagoslakeid == llid)
# states_bbox[[i]]$states
# ep <- dplyr::filter(ep, lagoslakeid %in% as.numeric(llid))

# pre-compute states and bboxes before conda corrupts rgdal
if(nrow(ep) == 0){
  quit("yes")
}
pb <- progress_bar$new(format = "  pulling bbox for :llid [:bar]", 
                       total = nrow(ep), 
                       clear = FALSE)
states_bbox <- list()
for(i in seq_len(length(ep$lagoslakeid))){
  llid <- ep$lagoslakeid[i]
  pb$tick(tokens = list(llid = llid))
  boundary_iws <- get_iws(llid)
  bbox         <- get_bbox(boundary_iws)
  states       <- suppressMessages(get_states(bbox))
  bbox_latlon  <- as.numeric(get_bbox(st_transform(boundary_iws, 4326)))
  states_bbox[[i]] <- list(boundary_iws = boundary_iws,
                           bbox = bbox, 
                           states = states,
                           bbox_latlon = bbox_latlon)
}

library(reticulate)
use_condaenv("gSSURGO")
gssurgo <- import("gssurgo")

pb <- progress_bar$new(format = "  pulling aoi tif for :llid [:bar]", 
                       total = nrow(ep), 
                       clear = FALSE)
for(i in seq_len(length(ep$lagoslakeid))){
  llid <- ep$lagoslakeid[i]
  pb$tick(tokens = list(llid = llid))
  
  # i <- 1
  # states_bbox[[i]]$states
  
  in_tifs <- list.files(in_path, pattern = "*.tif$", 
                        full.names = TRUE, include.dirs = TRUE)
  in_tifs <- in_tifs[grep(paste(states_bbox[[i]]$states,
                                collapse = "|"), in_tifs)] 
  iws_raster_path <- paste0(out_path, paste0(llid, "_", length(in_tifs), ".tif"))
  
  if(!file.exists(iws_raster_path) & length(in_tifs) > 0){
    
    gssurgo$aoi(in_path, iws_raster_path, 
                xmin = states_bbox[[i]]$bbox_latlon[1], 
                ymin = states_bbox[[i]]$bbox_latlon[2], 
                xmax = states_bbox[[i]]$bbox_latlon[3], 
                ymax = states_bbox[[i]]$bbox_latlon[4])
    
    iws_raster <- raster(iws_raster_path)
    iws_raster <- mask(iws_raster, states_bbox[[i]]$boundary_iws)
    writeRaster(iws_raster, iws_raster_path, overwrite = TRUE)
  }
}

write.csv(ep, "data/gssurgo/gssurgo.csv", row.names = FALSE)
