library(rnassqs)
library(dplyr)
library(maps)
library(sf)
library(tidyr)

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
                       county = tolower(county_name)) %>% 
  filter(!is.na(county) & nchar(county) > 0) %>%
  dplyr::select(state, county, Value, short_desc, domain_desc)

county <- st_as_sf(maps::map("county", fill = TRUE))
county <- tidyr::separate(county, ID, c("state", "county"))

county <- filter(county, ID %in% 
                   filter(county.fips, fips %in% test2$county_code)[,"polyname"])
