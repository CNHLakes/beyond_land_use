library(raster)

source("scripts/utils.R")

ep       <- readRDS("data/ep.rds")
in_path  <- path.expand("~/Documents/Science/Data/gssurgo_data/tifs")
out_path <- path.expand("~/Documents/Science/Data/gssurgo_data/aois/")

existing_rasters <- list.files(out_path, pattern = "\\d*_\\d.tif", 
                               include.dirs = TRUE, full.names = TRUE)
existing_llids <- stringr::str_extract(existing_rasters, "(\\d*)(?=_\\d.tif)")

# unlink(existing_rasters[!(existing_llids %in% ep$lagoslakeid)])
ep <- dplyr::filter(ep, !(lagoslakeid %in% as.numeric(existing_llids)))

for(i in seq_len(length(ep$lagoslakeid))){
  llid <- ep$lagoslakeid[i]
  print(llid)
  
  # i <- 1
  # llid <- 23670
  # llid <- 1935
  # llid <- 5331
  # llid <- 7483
  boundary_iws <- get_iws(llid)
  bbox         <- get_bbox(boundary_iws)
  states       <- suppressMessages(get_states(bbox))

  in_tifs <- list.files(in_path, pattern = "*.tif$", 
                        full.names = TRUE, include.dirs = TRUE)
  in_tifs <- in_tifs[grep(paste(states, collapse = "|"), in_tifs)] 
  
  iws_raster_path <- paste0(out_path, paste0(llid, "_", length(in_tifs), ".tif"))
  
  if(!file.exists(iws_raster_path) & length(in_tifs) > 0){
    if(length(in_tifs) > 1){ # clip from each and merge
      print("clip from first")
      system(paste0("gdal_translate -projwin ",
                    paste0(
                      as.vector(bbox)[c(1, 4, 3, 2)], collapse = " "),
                    " -of GTiff ",
                    in_tifs[1], " ",
                    "data/gssurgo/temp1.tif"))
      
      print("clip from second")
      system(paste0("gdal_translate -projwin ",
                    paste0(
                      as.vector(bbox)[c(1, 4, 3, 2)], collapse = " "),
                    " -of GTiff ",
                    in_tifs[2], " ",
                    "data/gssurgo/temp2.tif"))
      
      print("mosaic")
      system(paste0("gdal_merge.py -o " , iws_raster_path, " -n 0.0 ",
                    paste0(c("data/gssurgo/temp1.tif", 
                             "data/gssurgo/temp2.tif"), collapse = " ")))
    }else{ # clip from first
      gdal_string <- paste0("gdal_translate -projwin ",
                            paste0(
                              as.vector(bbox)[c(1, 4, 3, 2)], collapse = " "),
                            " -of GTiff ",
                            in_tifs, " ",
                            iws_raster_path)
      # print(gdal_string)
      system(gdal_string)
    }
    
    iws_raster <- raster(iws_raster_path)
    iws_raster <- mask(iws_raster, boundary_iws)
    writeRaster(iws_raster, iws_raster_path, overwrite = TRUE)
  }
}

write.csv(ep, "data/gssurgo/gssurgo.csv", row.names = FALSE)
