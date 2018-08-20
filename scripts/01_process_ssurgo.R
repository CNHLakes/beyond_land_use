# ---- setup ----

library(raster)
library(sf)

library(reticulate)
use_condaenv("gssurgo")
gssurgo <- import("gssurgo")

gpkg_path   <- path.expand("~/Dropbox/Data/gSSURGO/gpkgs/")
ssurgo_path <- "data/gssurgo/"

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

r_list <- list.files(ssurgo_path, pattern = "^\\d*_\\d.tif$", 
                     include.dirs = TRUE, full.names = TRUE)

# drop strange iws
r_list <- r_list[!(seq_len(length(r_list)) %in% grep(6198, r_list))]
llids  <- stringr::str_extract(r_list, "(\\d*)(?=_)")

res <- list()

for(i in seq_len(length(r_list))){
  # llid <- 6198; src_tif  <- r_list[grep(llid, r_list)[1]]; i <- 1
  src_tif       <- r_list[i]
  print(src_tif)
  base_r        <- suppressMessages(raster(src_tif))

  gssurgo$query_gpkg(src_tif, gpkg_path, qry, file.path(ssurgo_path, "temp.tif"))
                     
  data_r           <- suppressMessages(raster(file.path(ssurgo_path, "temp.tif")))
  # suppressMessages(mapview::mapview(r) + mapview::mapview(res))
  res_values     <- data_r[]
  iws_cell_n     <- sum(!is.na(base_r[])) - sum(res_values == 999, na.rm = TRUE)
  res_values     <- res_values[!is.na(res_values) & res_values != 999] / 100
  total_wetland  <- sum(res_values)
  (wetland_pct   <- (total_wetland / iws_cell_n) * 100)
  res[[i]]       <- wetland_pct
}

res <- data.frame(llid = llids, wetland_pct = unlist(res), 
                  stringsAsFactors = FALSE)

saveRDS(res, "data/gssurgo/gssurgo.rds")
