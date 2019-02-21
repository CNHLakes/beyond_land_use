cmdargs <- commandArgs(trailingOnly = TRUE)

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

pull_network <- function(x){
  network <- tryCatch(suppressWarnings(
    suppressMessages(
      extract_network(lon = x[1], 
                      lat = x[2], 
                      maxsteps = Inf))), 
    error = function(e){
      NA
    })
  if(!exists("network")){
    network <- NA
  }
  
  network
}

get_buffer_stats <- function(llid){
  # llid <- 5424
  # LAGOSNE::lake_info(llid)
  # llid <- ep$lagoslakeid[1]
  ll_pnt  <- st_coordinates(
    st_transform(
      query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", "lagoslakeid", llid), 4326))
  
  message("Pulling lake buffer........")
  ll_lake <- pull_lake_buffer(llid)
  
  message("Pulling stream buffer......")
  network <- pull_network(ll_pnt)
  
  message("Calculating bounding box...")
  if(all(class(network) != "logical")){
    has_streams   <- TRUE
    network       <- dplyr::filter(network, ftype != "ArtificialPath")
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
  
  message("Pulling NLCD...............")
  nlcd              <- suppressMessages(
                        get_nlcd(template = as_Spatial(buffer_aoi), 
                        label = llid))
  
  # suppressMessages(beginCluster())
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
  # endCluster()
  
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
# llid <- 4790
# test <- get_buffer_stats(llid)

# output <- read.csv("data/buffer_stats.csv", stringsAsFactors = FALSE)
# llids  <- ep$lagoslakeid[!(ep$lagoslakeid %in% output$llid)]
# llids <- 4790
if(exists("cmdargs")){
  if(length(cmdargs) > 0){
    llids <- cmdargs[1:length(cmdargs)]
  }
  if(length(grep("/", llids)) > 0){
    llids <- 
      sapply(llids, function(x){
      x <- strsplit(x, "/")
      x <- x[[1]][length(x[[1]])]
      gsub(".csv", "", x)
      })
  }
}

res <- list()
for(llid in llids){
  message(paste0("lagoslakeid: ", llid))
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
              file = paste0("data/buffer_lulc/", llid, ".csv"), append = TRUE, 
              sep = ",", row.names = FALSE, col.names = TRUE))
}
