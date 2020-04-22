source("scripts/99_utils.R")

# compute per huc tile drainage
# load huc4s
study_bbox    <- st_read("data/gis.gpkg", "study_bbox", quiet = TRUE)[3,]
hu4_focal     <- st_read("data/gis.gpkg", "hu4s_focal_simple", quiet = TRUE)
hu4_focal     <- st_crop(st_transform(hu4_focal, st_crs(study_bbox)), study_bbox)
# http://dx.doi.org/10.5066/F7W37TDP
library(exactextractr)
r <- raster("data/SubsurfaceDrainExtentMW_2012.tif")
test <- st_cast(hu4_focal, "MULTIPOLYGON")
test$tile_drainage <- exact_extract(r, test, "sum")
saveRDS(test, "data/tile_drainage.rds")
