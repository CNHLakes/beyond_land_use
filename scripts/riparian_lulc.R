library(nhdR)
library(LAGOSNEgis)
library(mapview)
library(FedData)
library(dplyr)
library(ggplot2)
library(raster)

dt <- readRDS("data/dt.rds")
source("scripts/utils.R")

llid <- dt$lagoslakeid[2]
ll_pnt <- st_coordinates(
  st_transform(
    query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", "lagoslakeid", llid), 4326))
ll_lake <- st_transform(
  query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", llid), 4326)
network <- extract_network(lon = ll_pnt[1], 
                                   lat = ll_pnt[2], 
                                   maxsteps = Inf)
network <- network[unlist(lapply(
  st_intersects(network, st_transform(ll_lake, st_crs(network))), 
  function(x) length(x) == 0)),]

network_buffer <- st_buffer(network, 100)
nlcd <- get_nlcd(template = as_Spatial(st_as_sfc(st_bbox(network_buffer))), 
                 label = llid)

join_key <- function(vals){
  res <- vals %>%
    data.frame(code = ., stringsAsFactors = FALSE) %>% 
    count(code) %>%
    left_join(FedData::pal_nlcd(), by = "code") %>%
    dplyr::filter(code != 11)
  res$description <- factor(res$description, 
                            levels = res$description)
  res
} 

buffer_stats <- join_key(as.character(
  unlist(raster::extract(r, network_buffer))))
nlcd_stats <- join_key(as.character(values(nlcd)))  
  
ggplot() + 
  geom_col(data = nlcd_stats, aes(x = description, 
                             y = n, 
                             fill = description)) +
  scale_fill_manual(values = nlcd_stats$color) +
  theme(axis.text.x = element_text(angle = 90))

ggplot() + 
  geom_col(data = buffer_stats, aes(x = description, 
                                  y = n, 
                                  fill = description)) +
  scale_fill_manual(values = buffer_stats$color) +
  theme(axis.text.x = element_text(angle = 90))

# mapview(nlcd, col.regions = test2$color, 
#         query.type = "click", label = TRUE) +
#   mapview(ll_lake) +
#   mapview(network_buffer)
  