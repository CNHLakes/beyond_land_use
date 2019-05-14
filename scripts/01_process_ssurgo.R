# ---- setup ----
suppressMessages(library(raster))
library(sf)
library(progress)
suppressMessages(library(dplyr))

Sys.setenv(RETICULATE_PYTHON = 
             "/home/jose/anaconda3/envs/gSSURGO/bin/python")
library(reticulate)
use_condaenv("gSSURGO", required = TRUE)
gssurgo <- import("gssurgo")

source("scripts/99_utils.R")
ep          <- readRDS("data/ep.rds")

gpkg_path   <- path.expand("~/Documents/Science/Data/gssurgo_data/gpkgs/")
aoi_path    <- path.expand("~/Documents/Science/Data/gssurgo_data/aois")
gssurgo_key <- read.csv("data/gssurgo/gssurgo_key.csv", stringsAsFactors = FALSE)

out_path    <- "data/gssurgo/gssurgo.rds"
if(file.exists(out_path)){
  res_disk    <- readRDS(out_path)
}

r_list <- list.files(aoi_path, pattern = "^\\d*_\\d.tif$", 
                     include.dirs = TRUE, full.names = TRUE)
llids  <- stringr::str_extract(r_list, "(\\d*)(?=_\\d.tif)")

pull_metric <- function(col_name, qry, agg_type, r_list, llids, res_disk, gssurgo_key,
                        overwrite = FALSE){
  raw_r_list <- r_list
  raw_llids  <- llids
  
  if(file.exists(out_path) & !overwrite){
    qry_changed <- qry != res_disk$gssurgo_key[
                            res_disk$gssurgo_key$metric == col_name, 
                            "query"]
    # print(paste0(col_name, " query has changed: ", qry_changed))
    
    if(col_name %in% names(res_disk$res) & !qry_changed){
      missing_llids <- !(llids %in% res_disk$res[,!is.na(col_name)][,"llid"])
      llids         <- raw_llids[missing_llids]
      r_list        <- raw_r_list[missing_llids]
    
      if(sum(missing_llids) == 0){
        return(setNames(data.frame(llid = raw_llids, 
                                 value = res_disk$res[,col_name], 
                                 stringsAsFactors = FALSE), 
                      c("llid", col_name)))
      }
    }
  }
  
  res <- list()
  pb  <- progress_bar$new(format = "  pulling :stat for :llid [:bar]", 
                         total = length(r_list), 
                         clear = FALSE)
  for(i in seq_len(length(r_list))){
    # i <- 1
    pb$tick(tokens = list(stat = col_name, llid = llids[i]))
    # llid <- 6198; src_tif  <- r_list[grep(llid, r_list)[1]]; i <- 1
    src_tif       <- r_list[i]
    # print(src_tif)
    base_r        <- suppressMessages(raster(src_tif))
    
    qry           <- file.path("/home/jose/Documents/Science/Dissertation/Analysis/_episodes_rmd/lagos_ag/data/gssurgo", paste0(col_name, ".sql"))
    
    py_string <- paste0("gssurgo.query_gpkg('", src_tif,
           "', '", gpkg_path,
           "', '", qry,
           "', '", file.path(aoi_path, "temp.tif')"))
    
    writeLines(c("import gssurgo", py_string), "temp.py")    
    system(paste0(Sys.getenv("RETICULATE_PYTHON"), " temp.py"))
    
    data_r           <- suppressMessages(raster(file.path(aoi_path, "temp.tif")))
    
    res_values     <- data_r[]
    # print(head(res_values))
    iws_cell_n     <- sum(!is.na(base_r[])) - sum(res_values == 999, na.rm = TRUE)
    res_values     <- res_values[!is.na(res_values) & res_values != 999]
    total    <- sum(res_values, na.rm = TRUE)
    pct      <- (total / iws_cell_n)
    res[[i]] <- pct
  }
  
  setNames(data.frame(llid = llids, 
             value = unlist(res), 
             stringsAsFactors = FALSE), c("llid", col_name))
}

# ---- execute ---- 
res <- apply(gssurgo_key, 1, function(x) 
  pull_metric(x[1], x[2], x[3], r_list, llids, res_disk, gssurgo_key))



res <- dplyr::bind_rows(res) %>% 
  group_by(llid) %>%
  arrange(llid) %>%
  tidyr::fill(wetland_potential:clay_pct) %>%
  na.omit %>%
  ungroup %>%
  data.frame(stringsAsFactors = FALSE)

if(file.exists(out_path)){
  res <- bind_rows(res_disk$res, res[!(res$llid %in% res_disk$res$llid),])
}

saveRDS(list(res = res, gssurgo_key = gssurgo_key), out_path)
# res <- readRDS(out_path)
