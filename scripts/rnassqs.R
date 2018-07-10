library(ggplot2)
library(rnassqs)
library(dplyr)
library(maps)
library(sf)
library(tidyr)
library(stringr)

county <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
county <- tidyr::separate(county, ID, c("state", "county"))

key <- "E44D2FCF-E267-3DE1-950A-E6C54EEA7058"

params <- list("source_desc" = "CENSUS", 
               "commodity_desc"="CORN", 
               "year__GE"=2012, 
               "state_alpha"="MI", 
               "domain_desc" = "TOTAL",
               "util_practice_desc" = "GRAIN",
               "agg_level_desc" = "COUNTY",
               "unit_desc" = "ACRES",
               key = key)
mi_corn      <- nassqs(params)

mi_corn_tidy <- mutate(mi_corn, 
                       state = tolower(state_name), 
                       county = tolower(county_name),
                       value = as.numeric(gsub(",", "", .data$Value))) %>% 
  filter(!is.na(county) & nchar(county) > 0) %>%
  dplyr::select(state, county, value, short_desc, domain_desc) %>%
  group_by(county) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  left_join(county) 

# str_detect(short_desc, "IRRIGATED"))

ggplot() + 
  geom_sf(data = mi_corn_tidy, aes(fill = total)) +
  labs(fill = "2012 USDA Census \n Total corn (acres)")
