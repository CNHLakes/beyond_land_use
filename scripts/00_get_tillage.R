library(macroag)
library(LAGOSNE)
library(LAGOSextra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
      
lg <- lagosne_load()

tc <- tillage_ctic %>%
  left_join(dplyr::select(lg$hu8, hu8, hu8_zoneid), 
            by = c("huc8_n" = "hu8")) %>%
  drop_na("hu8_zoneid") %>%
  dplyr::filter(year == 2004 & crop == "allcrops")
  
hu8 <- LAGOSextra::query_gis("HU8", "ZoneID", unique(tc$hu8_zoneid))
hu8 <- dplyr::select(hu8, -wkt)
hu8 <- dplyr::left_join(hu8, tc, by = c("ZoneID" = "hu8_zoneid"))

write_sf(hu8, "data/macroag/tillage.gpkg")

# ggplot() + 
#   geom_sf(data = hu8, aes(fill = pctnotil))
# 
