source("scripts/99_utils.R")

ep     <- readRDS("data/ep.rds")

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


pb <- progress_bar$new(format = "  pulling buffer lulc for :llid [:bar]", 
                       total = nrow(ep), 
                       clear = FALSE)
res <- list()
for(i in seq_along(ep$lagoslakeid[1:3])){
  # llid <- ep$lagoslakeid[2]
  llid <- ep$lagoslakeid[i]
  pb$tick(tokens = list(llid = llid))
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
  nlcd           <- get_nlcd(template = as_Spatial(st_as_sfc(
    st_bbox(network_buffer))), label = llid)
  network_buffer <- st_transform(network_buffer, projection(nlcd))
  
  nlcd_df           <- as.data.frame(nlcd, xy = TRUE) %>% 
    dplyr::select(x, y, contains("Value"))
  nlcd_df[,3]       <- factor(nlcd_df[,3])
  names(nlcd_df)[3] <- "value"
  cols              <- dplyr::filter(pal_nlcd(), code %in% unique(nlcd_df[,3]))
  
  # g_map <- ggplot() + 
  #   geom_raster(data = nlcd_df, aes(x = x, y = y, fill = value)) +
  #   scale_fill_manual(values = cols$color) +
  #   geom_sf(data = network_buffer) +
  #   coord_sf(datum = NULL) +
  #   theme(legend.position = "none", 
  #         axis.text = element_blank(), 
  #         axis.title = element_blank(), 
  #         line = element_blank(), 
  #         rect = element_blank(), 
  #         text = element_blank(), 
  #         panel.grid = element_blank())

  buffer_stats <- join_key(as.character(
    unlist(raster::extract(nlcd, network_buffer)))) %>%
    mutate(llid = llid)
  nlcd_stats <- join_key(as.character(values(nlcd)))  

  res[[i]] <- buffer_stats
}

res_all <- dplyr::bind_rows(res)

ggplot() + 
  geom_col(data = res_all, aes(x = description, 
                                  y = n, 
                                  fill = description)) +
  facet_wrap(~llid)
  
