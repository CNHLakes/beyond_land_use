library(assertr)
library(purrr)
library(readr)
suppressMessages(library(dplyr))
library(LAGOSNE)

ep                     <- readRDS("data/ep.rds")
buffer_metadata_folder <- "data/buffer_lulc/"
stream_lulc            <- read.csv("data/stream_buffer_stats.csv", 
                                   stringsAsFactors = FALSE)
lake_lulc              <- read.csv("data/lake_buffer_stats.csv", 
                                   stringsAsFactors = FALSE)

# read.csv(list.files(buffer_metadata_folder, full.names = TRUE)[14])

buffer_metadata <- buffer_metadata_folder %>% 
  list.files(full.names = TRUE) %>%
  map_df(function(x) read_csv(x, skip = 1, 
                              col_names = FALSE, col_types = "cccc")[1,]) %>%
  setNames(c("llid", "lake_buffer_area", "stream_buffer_area", 
             "stream_length")) %>%
  mutate_all(as.numeric) %>%
  verify(nrow(.) == nrow(ep)) %>%
  verify(all(ep$lagoslakeid %in% llid)) %>%
  verify(!any(is.na(as.numeric(stream_length))))

summarize_lulc <- function(x){
  # tidy global lulc object
  x_tidy <- x %>%
    dplyr::filter(llid != "llid") %>%
    dplyr::filter(code != 0) %>%
    distinct(llid, description, .keep_all = TRUE) %>%
    mutate(llid = as.numeric(llid))
  
  # calculate total lulc cell area
  area_totals <- group_by(x_tidy, llid) %>%
    summarize(total_n = sum(as.numeric(n))) %>% 
    left_join(buffer_metadata, by = "llid") %>%
    arrange(desc(total_n)) %>%
    mutate(total_area = total_n * 1000)
  
  left_join(x_tidy, area_totals, by = "llid") %>%
    mutate(n = as.numeric(n)) %>%
    mutate(percent = (n / total_n) * 100)
}

lake_lulc_tidy   <- summarize_lulc(lake_lulc) %>%
  rename(percent_lake = percent)
stream_lulc_tidy <- summarize_lulc(stream_lulc) %>%
  rename(percent_stream = percent) %>%
  dplyr::select(llid, description, percent_stream)

lulc_tidy <- full_join(lake_lulc_tidy, stream_lulc_tidy, 
                       by = c("llid", "description"))
  
# View(dplyr::filter(stream_lulc_tidy, llid == 4717))
# View(dplyr::filter(lulc_tidy, llid == 4717))
  
write.csv(lulc_tidy, "data/buffer_lulc.csv", row.names = FALSE)

# verify that numbers look correct
# source("scripts/99_utils.R")
# 
# ll_info <- lake_info(4717)
# 
# test <- pull_lake_buffer(4717)
# mapview::mapview(test$lake_buffer) + mapview::mapview(test$ll_lake)
# 
# ll_info$lake_area_ha
# st_area(test$lake_buffer); st_area(test$ll_lake);
# dplyr::filter(lake_lulc_tidy, llid == 4717)[1,]
# ll_info$lake_perim_meters
# View(dplyr::filter(lake_lulc_tidy, llid == 4717))
# 
# 
# ll_pnt  <- st_coordinates(
#   st_transform(
#     query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", "lagoslakeid", 4717), 4326))
# 
# test <- pull_network(ll_pnt)
# mapview(test)
# sum(st_length(test))
# View(dplyr::filter(stream_lulc_tidy, llid == 4717))
