cmdargs <- commandArgs(trailingOnly = TRUE)
cdl_path <- cmdargs[1]
states <- cmdargs[2:length(cmdargs)]

library(cdlTools)
library(LAGOSNE)
library(sf)
library(maps)

library(raster)
library(mapview)
library(reticulate)
library(dplyr)
library(tabularaster)
library(ggplot2)
library(gdalUtils)
library(LAGOSextra)

albers_conic_usgs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

# read ep data
lg <- lagosne_load("1.087.1")
ep <- readRDS("data/ep.rds")
ep <- left_join(ep, 
                dplyr::select(lg$locus, 
                              nhd_long, nhd_lat, lagoslakeid), 
                by = "lagoslakeid")
ep <- coordinatize(ep)
ep <- st_transform(ep, albers_conic_usgs)
ep_bbox <- as.vector(st_bbox(ep))

cdl <- getCDL(year = 2012, bbox = ep_bbox)

# make sf states object
# state_sf <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
# key <- data.frame(ID = tolower(state.name), 
#                   ABB = state.abb, stringsAsFactors = FALSE)
# state_sf <- left_join(state_sf, key, by = "ID")
# state_sf <- st_transform(state_sf, albers_conic_usgs)

cdl_mi <- cdlTools::getCDL(year = 2012, bbox = , ssl.verifypeer = FALSE)

writeRaster(cdl_mi$MI2012, paste0(cdl_path, "cdl_mi.tif"))

system(paste("gdalwarp -tr 120 120 -r average",
             paste0(cdl_path, "cdl_mi.tif"),
             paste0(cdl_path, "cdl_mi_aggregate.tif")))

# ---- cdl_waffle ----
# setwd("_episodes_rmd")
cdl_mi <- raster(paste0(cdl_path, "cdl_mi_aggregate.tif"))

cdl_table <- as_data_frame(table(values(cdl_mi)), stringsAsFactors = FALSE) %>%
  mutate(Var1 = as.integer(Var1))

cdl_key <- data.frame(code = 0:255, description = cdlTools::updateNamesCDL(0:255),
                      stringsAsFactors = FALSE)
cdl_key <- cdl_key[which(nchar(cdl_key$description) > 3),]
aggregate_categories <- function(cdl_key){
  res <- mutate(cdl_key, category = case_when(
    grepl("wheat", tolower(description)) ~ "wheat",
    grepl("wetlands", tolower(description)) ~ "wetlands",
    grepl("developed", tolower(description)) ~ "developed",
    grepl("dbl", tolower(description)) ~ "mixed crop",
    grepl("background", tolower(description)) ~ "background",
    grepl("barren", tolower(description)) ~ "other non ag",
    grepl("corn", tolower(description)) ~ "corn",
    grepl("soybeans", tolower(description)) ~ "soybeans",
    grepl("alfalfa", tolower(description)) ~ "alfalfa",
    grepl("pasture", tolower(description)) ~ "pasture",
    grepl("grassland", tolower(description)) ~ "other non ag",
    grepl("shrubland", tolower(description)) ~ "other non ag",
    grepl("forest", tolower(description)) ~ "forest",
    grepl("nonag", tolower(description)) ~ "other non ag",
    grepl("water", tolower(description)) ~ "water",
    grepl("aqua", tolower(description)) ~ "other non ag",
    grepl("ice", tolower(description)) ~ "other non ag",
    grepl("clouds", tolower(description)) ~ "other non ag"
  )) %>%
    tidyr::replace_na(list(category = "other ag"))

  res <- mutate(res, is_ag = if_else(category %in% c("corn", "wheat", "other ag",
                                                     "soybeans", "mixed crop", "pasture",
                                                     "alfalfa"),
                                     "ag", "nonag"))
  res
}

cdl_key <- aggregate_categories(cdl_key)

cdl_freq <- left_join(cdl_key, cdl_table, by = c("code" = "Var1")) %>%
  filter(description != "Background") %>%
  arrange(desc(n)) %>%
  group_by(category) %>%
  summarize(group_n = sum(n, na.rm = TRUE), is_ag = first(is_ag)) %>%
  ungroup() %>%
  mutate(percent = round((group_n / sum(group_n, na.rm = TRUE)) * 100, 0)) %>%
  filter(percent >= 1) %>%
  arrange(is_ag, percent)
cdl_freq <- add_row(cdl_freq, category = "other", group_n = NA,
                    percent = 100 - sum(cdl_freq$percent, na.rm = TRUE))

category_order <- arrange(cdl_freq, is_ag, percent)$category

cdl_isag <- left_join(cdl_key, cdl_table, by = c("code" = "Var1")) %>%
  filter(description != "Background") %>%
  arrange(desc(n)) %>%
  group_by(is_ag) %>%
  summarize(group_n = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent = round((group_n / sum(group_n, na.rm = TRUE)) * 100, 0)) %>%
  arrange(percent)