library(raster)
library(sf)

library(reticulate)
use_condaenv("gssurgo")
gssurgo <- import("gssurgo")

gpkg_path <- path.expand("~/Dropbox/Data/gSSURGO/gpkgs/")
ssurgo_path <- "data/gssurgo/"

source("scripts/utils.R")

r_list <- list.files(ssurgo_path, pattern = "^\\d*_\\d.tif$", 
                     include.dirs = TRUE, full.names = TRUE)

# llid <- 6874
# llid <- 23670

llid <- 34377
src_tif <- r_list[grep(llid, r_list)[1]]
in_gpkg <- list.files(gpkg_path, pattern = ".gpkg", 
                      full.names = TRUE, include.dirs = TRUE)[1]

r      <- raster(src_tif)
# r2 <- raster("/home/jose/python/gssurgo/tests/nonirryield_r.tif")
# mapview::mapview(r) +  mapview::mapview(r2)
# bbox   <- st_bbox(r)
# st_bbox(st_transform(st_as_sfc(bbox), 4326))
# states <- get_states(bbox)


# ---- construct query ----
con <- src_sqlite(in_gpkg)
# src_tbls(con)
qry <- tbl(con, "Valu1") %>%
  select(mukey, pwsl1pomu) %>%
  dbplyr::sql_render() %>%
  stringr::str_replace_all(c("`" = "", "\n" = " "))

# ---- loop through gpkgs ----
# call gssurgo python for each
# read to temp tif

gssurgo$query_gpkg(src_tif, gpkg_path, qry, file.path(ssurgo_path, "temp.tif"))
                     
res <- raster(file.path(ssurgo_path, "temp.tif"))

mapview::mapview(r) + mapview::mapview(res)