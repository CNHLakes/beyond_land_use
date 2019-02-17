library(assertr)
library(purrr)
library(readr)
library(dplyr)
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
  verify(nrow(.) == nrow(ep)) %>%
  verify(all(ep$lagoslakeid %in% llid)) %>%
  verify(!any(is.na(as.numeric(stream_length))))

buffer_summary <- function(x){
  unique(lake_lulc$description)
  unique(stream_lulc$description)
  
  # x <- lake_lulc
  test <- x %>%
    dplyr::filter(llid != "llid") %>%
    distinct(llid, description, .keep_all = TRUE)
  
  test2 <- group_by(test, llid) %>%
    summarize(total_n = sum(as.numeric(n))) %>%
    arrange(desc(total_n))
  
  lake_info(4717)
  
  test3 <- left_join(test2, buffer_metadata)
  
  test2[1,]$total_n * 30
  
  dplyr::filter(buffer_metadata, llid == test2[1,]$llid)
  
  
}

