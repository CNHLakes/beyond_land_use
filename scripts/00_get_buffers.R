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

get_buffer_stats <- function(llid){
  # llid <- 2167
  ll_pnt <- st_coordinates(
    st_transform(
      query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", "lagoslakeid", llid), 4326))
  
  # pull lake buffer
  ll_lake <- st_transform(
    query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", llid), 4326)
  lake_buffer      <- st_buffer(
    st_transform(ll_lake, nhdR:::albers_conic()), 
    units::as_units(100, "m"))
  lake_buffer      <- st_difference(lake_buffer, st_transform(
    fill_holes(ll_lake, 10000), nhdR:::albers_conic()))
  lake_buffer_area <- sum(st_area(lake_buffer))
  
  # pull stream network buffer
  network <- suppressWarnings(suppressMessages(extract_network(lon = ll_pnt[1], 
                                                               lat = ll_pnt[2], 
                                                               maxsteps = Inf)))
  if(all(class(network) != "logical")){
    network <- network[unlist(lapply(
      st_intersects(network, st_transform(ll_lake, st_crs(network))), 
      function(x) length(x) == 0)),]
    has_streams <- TRUE
    stream_length <- sum(st_length(network))
    
    network_buffer <- st_buffer(
      st_transform(network, nhdR:::albers_conic()), 
      units::as_units(100, "m"))
    stream_buffer_area <- sum(st_area(network_buffer))
  }else{
    has_streams   <- FALSE
    stream_length <- 0
    stream_buffer_area   <- 0
  }
  
  # pull nlcd
  buffer_aoi <- st_union(st_as_sfc(
    st_bbox(network_buffer)), 
    st_as_sfc(
      st_bbox(lake_buffer)))
  buffer_aoi <- st_transform(buffer_aoi, 4326)
  
  nlcd           <- get_nlcd(template = as_Spatial(buffer_aoi), label = llid)
  
  nlcd_df           <- as.data.frame(nlcd, xy = TRUE) %>% 
    dplyr::select(x, y, contains("Value"))
  nlcd_df[,3]       <- factor(nlcd_df[,3])
  names(nlcd_df)[3] <- "value"
  cols              <- dplyr::filter(pal_nlcd(), code %in% unique(nlcd_df[,3]))
  
  # construct plot for debugging
  g_map <- ggplot() +
    geom_raster(data = nlcd_df, aes(x = x, y = y, fill = value)) +
    scale_fill_manual(values = cols$color) +
    geom_sf(data = network_buffer) +
    coord_sf(datum = NULL) +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          line = element_blank(),
          rect = element_blank(),
          text = element_blank(),
          panel.grid = element_blank())
  
  beginCluster()
  # pull stream buffer nlcd
  if(has_streams){
    stream_buffer_stats <- join_key(as.character(
      unlist(raster::extract(nlcd, network_buffer)))) %>%
      mutate(llid = llid)
  }else{
    stream_buffer_stats <- NA
  }
  
  # pull lake buffer nlcd
  lake_buffer_stats <- join_key(as.character(
    unlist(raster::extract(nlcd, lake_buffer)))) %>%
    mutate(llid = llid)
  endCluster()
  
  # save bbox nlcd
  nlcd_stats             <- join_key(as.character(values(nlcd)))
  
  list(nlcd_stats = nlcd_stats, 
       stream_buffer_stats = stream_buffer_stats, 
       stream_length = stream_length,
       stream_buffer_area = stream_buffer_area,
       lake_buffer_stats = lake_buffer_stats, 
       lake_buffer_area = lake_buffer_area,
       g_map = g_map)
}

# llid <- ep$lagoslakeid[1]
# test <- get_buffer_stats(llid)

pb <- progress_bar$new(format = "  pulling buffer lulc for :llid [:bar]", 
                       total = nrow(ep), 
                       clear = FALSE)
pb$tick(0)
res <- list()
for(i in seq_along(ep$lagoslakeid[1:10])){
  llid          <- ep$lagoslakeid[i]
  pb$tick(tokens = list(llid = llid))
  res[[i]]      <- get_buffer_stats(llid)$nlcd_stats
  res[[i]]$llid <- llid
}

buffer_lulc <- suppressWarnings(dplyr::bind_rows(res))
saveRDS(buffer_lulc, "data/buffer_lulc.rds")

# ggplot() + 
#   geom_col(data = res_all, aes(x = description, 
#                                   y = n, 
#                                   fill = description)) +
#   facet_wrap(~llid)
  
