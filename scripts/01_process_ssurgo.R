# ---- setup ----
suppressMessages(library(raster))
library(sf)
library(progress)

library(reticulate)
use_condaenv("gssurgo")
gssurgo <- import("gssurgo")

gpkg_path   <- path.expand("~/Documents/Science/Data/gssurgo_data/gpkgs/")
aoi_path    <- path.expand("~/Documents/Science/Data/gssurgo_data/aois/")
gssurgo_key <- read.csv("data/gssurgo/gssurgo_key.csv")
out_path    <- "data/gssurgo/gssurgo.rds"
res_disk    <- readRDS(out_path)


source("scripts/utils.R")

ep <- readRDS("data/ep.rds")

# ---- execute ---- 
r_list <- list.files(aoi_path, pattern = "^\\d*_\\d.tif$", 
                     include.dirs = TRUE, full.names = TRUE)
llids  <- stringr::str_extract(r_list, "(\\d*)(?=_\\d.tif)")

res <- apply(gssurgo_key, 1, function(x) 
  pull_metric(x[1], x[2], r_list, llids, res_disk))

pull_metric <- function(col_name, qry, r_list, llids, res_disk){
  
  print(paste("in r_list length", length(r_list)))
  
  if(col_name %in% names(res_disk)){
    missing_llids <- !(llids %in% res_disk[,!is.na(col_name)][,"llid"])
    llids         <- llids[missing_llids]
    r_list        <- r_list[missing_llids]
    
    if(sum(missing_llids) == 0){
      return(res_disk[,col_name])
    }
  }
  
  res <- list()
  pb <- progress_bar$new(format = "  pulling stats for :llid [:bar]", 
                         total = length(r_list), 
                         clear = FALSE)
  for(i in seq_len(length(r_list))){
    # i <- 5
    pb$tick(tokens = list(llid = llids[i]))
    # llid <- 6198; src_tif  <- r_list[grep(llid, r_list)[1]]; i <- 1
    src_tif       <- r_list[i]
    # print(src_tif)
    base_r        <- suppressMessages(raster(src_tif))
  
    gssurgo$query_gpkg(src_tif, gpkg_path, qry, file.path(aoi_path, "temp.tif"))
                       
    data_r           <- suppressMessages(raster(file.path(aoi_path, "temp.tif")))
    # suppressMessages(mapview::mapview(r) + mapview::mapview(res))
    res_values     <- data_r[]
    # print(head(res_values))
    iws_cell_n     <- sum(!is.na(base_r[])) - sum(res_values == 999, na.rm = TRUE)
    res_values     <- res_values[!is.na(res_values) & res_values != 999] / 100
    total_wetland  <- sum(res_values)
    (wetland_pct   <- (total_wetland / iws_cell_n) * 100)
    res[[i]]       <- wetland_pct
  }
  print(paste("out r_list length", length(r_list)))
  unlist(res)
}

saveRDS(res, out_path)
