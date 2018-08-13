cmdargs <- commandArgs(trailingOnly = TRUE)
cdl_path <- cmdargs[1]
# gssurgo_path <- "data/gssurgo/"
in_path <- "~/Documents/Science/Data/gSSURGO/tifs"

source("scripts/utils.R")

for(i in seq_len(length(ep$lagoslakeid))){
  llid <- ep$lagoslakeid[i]
  print(llid)
  
  # i <- 1
  # llid <- 23670
  boundary_iws <- get_iws(llid)
  bbox         <- get_bbox(boundary_iws)
  states       <- get_states(bbox)

  in_tifs <- list.files(in_path, pattern = "*.tif$", 
                        full.names = TRUE, include.dirs = TRUE)
  in_tifs <- in_tifs[grep(paste(states, collapse = "|"), in_tifs)] 
    
  # mosaic gssurgo
  mosaic_path <- paste0(gssurgo_path, paste0(states, collapse = "_"), ".tif")
  if(!file.exists(mosaic_path)){
    system(
      paste0("gdal_merge.py -o " , mosaic_path, " ",
                  paste0(in_tifs, collapse = " "))
      )
  }
  
  iws_raster_path <- paste0(gssurgo_path, paste0(llid, ".tif"))
  
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