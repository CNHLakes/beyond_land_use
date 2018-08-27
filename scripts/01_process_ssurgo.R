# ---- setup ----
suppressMessages(library(raster))
library(sf)
library(progress)

library(reticulate)
use_condaenv("gssurgo")
gssurgo <- import("gssurgo")

gpkg_path <- path.expand("~/Documents/Science/Data/gssurgo_data/gpkgs/")
aoi_path  <- path.expand("~/Documents/Science/Data/gssurgo_data/aois/")

source("scripts/utils.R")

# ---- construct query ----
in_gpkg <- list.files(gpkg_path, pattern = ".gpkg", 
                      full.names = TRUE, include.dirs = TRUE)[1]
con <- src_sqlite(in_gpkg)
# src_tbls(con)

# percentage of the map unit that meets the criteria for 
# a potential wetland soil landscape
qry <- tbl(con, "Valu1") %>%
  select(mukey, pwsl1pomu) %>%
  dbplyr::sql_render() %>%
  stringr::str_replace_all(c("`" = "", "\n" = " "))

# ---- execute ---- 

r_list <- list.files(aoi_path, pattern = "^\\d*_\\d.tif$", 
                     include.dirs = TRUE, full.names = TRUE)

# drop strange iws
r_list <- r_list[!(seq_len(length(r_list)) %in% grep(6198, r_list))]
llids  <- stringr::str_extract(r_list, "(\\d*)(?=_\\d.tif)")

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

saveRDS(res, "temp.rds")

res <- data.frame(llid = as.integer(llids), wetland_pct = unlist(res), stringsAsFactors = FALSE)

saveRDS(res, "data/gssurgo/gssurgo.rds")
