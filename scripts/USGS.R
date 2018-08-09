# ---- usgs_setup -----

library(ggplot2)
library(sf)
library(readxl)
library(writexl)
library(dplyr)
library(magrittr)
library(janitor)

# ---- usgs_viz ----
# setwd("_episodes_rmd")

usgs <- readRDS("lagos_ag/data/usgs.rds")

county_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
county_sf <- tidyr::separate(county_sf, ID, c("state", "county"))
county_mi <- filter(county_sf, state == "michigan")

usgs_mi <- filter(usgs, state == "MI", year == 1997) %>%
  mutate(county = tolower(county)) %>%
  group_by(county, variable, year) %>%
  summarize(total = sum(as.numeric(value), na.rm = TRUE)) # %>%
  # spread(variable, total)

usgs_mi <- right_join(county_mi, usgs_mi, by = c("county"))

ggplot() +
  geom_sf(data = usgs_mi, aes(fill = log(total))) + facet_wrap(~variable)
