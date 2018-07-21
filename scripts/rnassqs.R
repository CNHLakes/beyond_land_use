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
Sys.setenv(NASSQS_TOKEN = key)

# ---- county_level_data ----

# See https://quickstats.nass.usda.gov/api#param_define for param descriptions
params <- list("source_desc" = "CENSUS", 
               "commodity_desc"="WHEAT", 
               "year"=2007, 
               "state_alpha"="MI", 
               "domain_des" = "TOTAL",
               "agg_level_desc" = "COUNTY",
               "unit_desc" = "ACRES",
               key = key)
mi_rye      <- nassqs(params)

mi_rye_tidy <- mi_rye %>% 
  mutate(state = tolower(state_name), 
         county = tolower(county_name),
         value = as.numeric(gsub(",", "", .data$Value))) %>% 
  filter(!is.na(county) & nchar(county) > 0 & year %in% params$year) %>%
  dplyr::select(state, county, value, short_desc, domain_desc) %>%
  group_by(county) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  left_join(county) 

# rm empty geometries and calculate areas
mi_rye_tidy <- mi_rye_tidy[unlist(lapply(st_geometry(mi_rye_tidy$geometry), "length")) == 1,]
mi_rye_tidy$area <- units::set_units(
  st_area(st_cast(mi_rye_tidy$geometry, "POLYGON")), "acres")
mi_rye_tidy <- mutate(mi_rye_tidy, percent_rye = total / area)

# str_detect(short_desc, "IRRIGATED"))

ggplot() + 
  geom_sf(data = mi_rye_tidy, aes(fill = total)) +
  labs(fill = "2012 USDA Census \n Total rye (acres)")

ggplot() + 
  geom_sf(data = mi_rye_tidy, aes(fill = percent_rye)) +
  labs(fill = "2012 USDA Census \n % rye")

# ---- HU6_level_data ----

gdb_path <- path.expand("~/.local/share/LAGOS-GIS/lagos-ne_gis.gpkg")
hu8         <- st_read(gdb_path, "HU8")
hu8 <- filter(hu8, str_detect(States, "MI"))
hu8$HUC6 <- paste0(sapply(as.character(hu8$HUC8), 
                          function(x) substring(x, 0, 6)), "00")

# See https://quickstats.nass.usda.gov/api#param_define for param descriptions
params <- list("source_desc" = "CENSUS",
               "watershed_code" = hu8$HUC6, 
               "commodity_desc" = "CORN",
               "unit_desc" = "ACRES",
               "year" = 2007,
               key = key)
test <- nassqs(params)
test <- left_join(test, hu8, by = c("location_desc" = "HUC6"))
test$Value <- as.numeric(gsub(",", "", test$Value))
test2 <- test %>% 
  st_sf() %>%
  group_by(location_desc) %>%
  summarize(hu6_corn_acres = mean(Value), short_desc = unique(short_desc))

saveRDS(test2, "data/mi_hu6_corn.rds")

ggplot() + 
  geom_sf(data = st_sf(test2), aes(fill = Value)) +
  labs(fill = "2012 USDA Census \n Corn (acres)")
