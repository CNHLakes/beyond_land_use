library(purrr)
library(readr)
library(dplyr)

ep                     <- readRDS("data/ep.rds")
buffer_metadata_folder <- "data/buffer_lulc/"
stream_lulc            <- "data/stream_buffer_stats.csv"
lake_lulc              <- "data/lake_buffer_stats.csv"

# read.csv(list.files(buffer_metadata_folder, full.names = TRUE)[14])

buffer_metadata <- buffer_metadata_folder %>% 
  list.files(full.names = TRUE) %>%
  map_df(function(x) read_csv(x, skip = 1, 
                              col_names = FALSE, col_types = "cccc")[1,]) %>%
  setNames(c("llid", "lake_buffer_area", "stream_buffer_area", 
             "stream_length"))

# nrow(buffer_metadata[which(is.na(as.numeric(buffer_metadata$stream_length))),]) == 0
# any(!(ep$lagoslakeid %in% buffer_metadata$llid)) == FALSE
# nrow(ep) == nrow(buffer_metadata)

buffer_summary <- function(){
  
}