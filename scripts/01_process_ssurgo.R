# ---- setup ----
suppressMessages(library(raster))
library(sf)
library(progress)

library(reticulate)
use_condaenv("gSSURGO")
gssurgo <- import("gssurgo")

gpkg_path   <- path.expand("~/Documents/Science/Data/gssurgo_data/gpkgs/")
aoi_path    <- path.expand("~/Documents/Science/Data/gssurgo_data/aois/")
gssurgo_key <- read.csv("data/gssurgo/gssurgo_key.csv", stringsAsFactors = FALSE)
out_path    <- "data/gssurgo/gssurgo.rds"
res_disk    <- readRDS(out_path)

source("scripts/utils.R")

ep <- readRDS("data/ep.rds")

# ---- execute ---- 
r_list <- list.files(aoi_path, pattern = "^\\d*_\\d.tif$", 
                     include.dirs = TRUE, full.names = TRUE)
llids  <- stringr::str_extract(r_list, "(\\d*)(?=_\\d.tif)")

pull_metric <- function(col_name, qry, agg_type, r_list, llids, res_disk){
  raw_r_list <- r_list
  raw_llids  <- llids
  # print(paste("in r_list length", length(r_list)))
  
  if(col_name %in% names(res_disk)){
    missing_llids <- !(llids %in% res_disk[,!is.na(col_name)][,"llid"])
    llids         <- llids[missing_llids]
    r_list        <- r_list[missing_llids]
    
    if(sum(missing_llids) == 0){
      return(setNames(data.frame(llid = raw_llids, 
                                 value = res_disk[,col_name]), 
                      c("llid", col_name)))
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
  
    # # echo python command
    # paste0("gssurog.query_gpkg(", src_tif, 
    #        " ", gpkg_path, 
    #        " ", qry,
    #        " ", file.path(aoi_path, "temp.tif"))
    gssurgo$query_gpkg(src_tif, gpkg_path, qry, file.path(aoi_path, "temp.tif"))
                       
    data_r           <- suppressMessages(raster(file.path(aoi_path, "temp.tif")))
    # suppressMessages(mapview::mapview(base_r) + mapview::mapview(data_r))
    res_values     <- data_r[]
    # print(head(res_values))
    iws_cell_n     <- sum(!is.na(base_r[])) - sum(res_values == 999, na.rm = TRUE)
    res_values     <- res_values[!is.na(res_values) & res_values != 999]
    if(agg_type == "pct"){ 
      res_values <- res_values / 100
      total    <- sum(res_values)
      pct      <- (total / iws_cell_n) * 100
    }else{
      total    <- sum(res_values, na.rm = TRUE)
      pct      <- (total / iws_cell_n)
    }
    res[[i]] <- pct
  }
  
  setNames(data.frame(llid = llids, 
             value = unlist(res)), c("llid", col_name))
}

res <- apply(gssurgo_key[3,], 1, function(x) 
  pull_metric(x[1], x[2], x[3], r_list[1], llids[1], res_disk[1,]))

saveRDS(res, out_path)
