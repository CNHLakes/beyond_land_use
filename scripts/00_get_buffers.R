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

pull_lake_buffer <- function(llid){
  ll_lake <- st_transform(
    query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", llid), 4326)
  lake_buffer      <- st_buffer(
    st_transform(ll_lake, nhdR:::albers_conic()), 
    units::as_units(100, "m"))
  lake_buffer      <- suppressWarnings(
    st_difference(lake_buffer, 
                  st_transform(
                    fill_holes(ll_lake, 10000), nhdR:::albers_conic())))
  lake_buffer_area <- sum(st_area(lake_buffer))
  
  list(ll_lake = ll_lake,
       lake_buffer = lake_buffer, 
       lake_buffer_area = lake_buffer_area)
}

pull_network <- function(ll_pnt){
  tryCatch(network <- suppressWarnings(
    suppressMessages(
      extract_network(lon = ll_pnt[1], 
                      lat = ll_pnt[2], 
                      maxsteps = Inf))), 
    error = function(e){network <- NA})
  if(!exists("network")){
    network <- NA
  }
  
  network
}

get_buffer_stats <- function(llid){
  # llid <- 4254
  # LAGOSNE::lake_info(llid)
  # llid <- ep$lagoslakeid[1]
  ll_pnt  <- st_coordinates(
    st_transform(
      query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", "lagoslakeid", llid), 4326))
  
  ll_lake <- pull_lake_buffer(llid)
  network <- pull_network(ll_pnt)
  
  if(all(class(network) != "logical")){
    has_streams   <- TRUE
    network       <- network[unlist(lapply(
      st_intersects(network, st_transform(ll_lake$ll_lake, st_crs(network))), 
      function(x) length(x) == 0)),]
    stream_length <- sum(st_length(network))
    
    network_buffer <- st_buffer(
      st_transform(network, nhdR:::albers_conic()), 
      units::as_units(100, "m"))
    stream_buffer_area <- sum(st_area(network_buffer))
    
    buffer_aoi <- st_union(st_as_sfc(
      st_bbox(network_buffer)), 
      st_as_sfc(
        st_bbox(ll_lake$lake_buffer)))
    buffer_aoi <- st_transform(buffer_aoi, 4326)
  }else{
    has_streams          <- FALSE
    network_buffer       <- NA
    stream_length        <- 0
    stream_buffer_area   <- 0
    
    buffer_aoi <- st_as_sfc(st_bbox(ll_lake$lake_buffer))
    buffer_aoi <- st_transform(buffer_aoi, 4326)
  }
  
  # pull nlcd
  nlcd              <- get_nlcd(template = as_Spatial(buffer_aoi), 
                                label = llid)
  
  # construct plot for debugging
  # nlcd_df           <- as.data.frame(nlcd, xy = TRUE) %>% 
  #   dplyr::select(x, y, contains("Value"))
  # nlcd_df[,3]       <- factor(nlcd_df[,3])
  # names(nlcd_df)[3] <- "value"
  # cols              <- dplyr::filter(pal_nlcd(), code %in% unique(nlcd_df[,3]))
  # 
  # if(all(class(network) != "logical")){
  #   g_map <- ggplot() +
  #     geom_raster(data = nlcd_df, aes(x = x, y = y, fill = value)) +
  #     scale_fill_manual(values = cols$color) +
  #     geom_sf(data = network_buffer) +
  #     coord_sf(datum = NULL) +
  #     theme(legend.position = "none",
  #           axis.text = element_blank(),
  #           axis.title = element_blank(),
  #           line = element_blank(),
  #           rect = element_blank(),
  #           text = element_blank(),
  #           panel.grid = element_blank())
  # }else{
  #   g_map <- ggplot() +
  #     geom_raster(data = nlcd_df, aes(x = x, y = y, fill = value)) +
  #     scale_fill_manual(values = cols$color) +
  #     geom_sf(data = lake_buffer) +
  #     coord_sf(datum = NULL) +
  #     theme(legend.position = "none",
  #           axis.text = element_blank(),
  #           axis.title = element_blank(),
  #           line = element_blank(),
  #           rect = element_blank(),
  #           text = element_blank(),
  #           panel.grid = element_blank())
  # }
  
  suppressMessages(beginCluster())
  # pull stream buffer nlcd
  if(has_streams){
    suppressMessages(stream_buffer_stats <- join_key(
      as.character(na.omit(values(raster::mask(nlcd, network_buffer))))) %>%
      mutate(llid = llid))
  }else{
    stream_buffer_stats <- setNames(data.frame(NA, NA, NA, NA, NA, llid), 
                                    c("code", "n", "class", "description", 
                                      "color", "llid"))
  }
  
  # pull lake buffer nlcd
  lake_buffer_stats <- join_key(
    as.character(na.omit(values(raster::mask(nlcd, ll_lake$lake_buffer))))) %>%
      mutate(llid = llid)
  endCluster()
  
  # save bbox nlcd
  nlcd_stats             <- join_key(as.character(values(nlcd)))
  
  list(nlcd_stats = nlcd_stats, 
       stream_buffer_stats = stream_buffer_stats, 
       stream_length = stream_length,
       stream_buffer_area = stream_buffer_area,
       lake_buffer_stats = lake_buffer_stats, 
       lake_buffer_area = ll_lake$lake_buffer_area)
}

# llid <- ep$lagoslakeid[5]
# llid <- 4254
# test <- get_buffer_stats(llid)

pb <- progress_bar$new(format = "  pulling buffer lulc for :llid [:bar]", 
                       total = nrow(ep), 
                       clear = FALSE)
res <- list()
invisible(pb$tick(0))
for(i in seq_along(ep$lagoslakeid[15:nrow(ep)])){
  llid          <- ep$lagoslakeid[i]
  pb$tick(tokens = list(llid = llid))
  res[[1]]      <- get_buffer_stats(llid)
  res[[1]]$llid <- llid

  suppressWarnings(
  write.table(res[[1]]$stream_buffer_stats, 
              file = "data/stream_buffer_stats.csv", append = TRUE, 
              sep = ",", row.names = FALSE, col.names = TRUE))
  
  suppressWarnings(
  write.table(res[[1]]$lake_buffer_stats, 
              file = "data/lake_buffer_stats.csv", append = TRUE, 
              sep = ",", row.names = FALSE, col.names = TRUE))
  
  suppressWarnings(
    write.table(res[[1]][c("llid", 
                         "lake_buffer_area", 
                         "stream_buffer_area", 
                         "stream_length")], 
              file = "data/buffer_stats.csv", append = TRUE, 
              sep = ",", row.names = FALSE, col.names = TRUE))
  
}

# stream_buffer_nlcd <- suppressWarnings(dplyr::bind_rows(
#   lapply(res, function(x) x$stream_buffer_stats)))
# 
# lake_buffer_nlcd <- suppressWarnings(dplyr::bind_rows(
#   lapply(res, function(x) x$lake_buffer_stats)))
# 
# buffer_stats <- suppressWarnings(dplyr::bind_rows(
#   lapply(res, function(x) x[c("llid", 
#                               "lake_buffer_area", 
#                               "stream_buffer_area", 
#                               "stream_length")])))
# 
# res_final <- list(buffer_stats = buffer_stats, 
#                   lake_buffer_nlcd = lake_buffer_nlcd, 
#                   stream_buffer_nlcd = stream_buffer_nlcd)
# 
# saveRDS(res_final, "data/buffer_lulc.rds")

# verify
# res <- readRDS("data/buffer_lulc.rds")
