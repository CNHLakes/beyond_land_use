library(ggplot2)
library(rnassqs)
library(dplyr)
library(maps)
library(sf)
library(tidyr)
library(stringr)

source("scripts/99_utils.R")

county <- county_sf()

key <- "E44D2FCF-E267-3DE1-950A-E6C54EEA7058"
Sys.setenv(NASSQS_TOKEN = key)

# ---- county_level_data ----

# See https://quickstats.nass.usda.gov/api#param_define for param descriptions
params <- list("source_desc" = "CENSUS",
               "commodity_desc"="WHEAT",
               "year"=2007,
               "state_alpha"="MI",
               "domain_desc" = "TOTAL",
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

# total animal counts
params <- list("source_desc" = "CENSUS",
               "commodity_desc"="CATTLE",
               "year" = 2007,
               "state_alpha" = "MI",
               "domain_desc" = "TOTAL",
               "agg_level_desc" = "COUNTY",
               # "unit_desc" = "ACRES",
               key = key)
mi_animals      <- nassqs(params)

mi_animals_tidy <- mi_animals %>%
  mutate(state = tolower(state_name),
         county = tolower(county_name),
         value = as.numeric(gsub(",", "", .data$Value))) %>%
  filter(!is.na(county) & nchar(county) > 0 & year %in% params$year) %>%
  dplyr::select(state, county, value, short_desc, domain_desc) %>%
  group_by(county) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  left_join(county)

# rm empty geometries and calculate areas
mi_animals_tidy <- mi_animals_tidy[unlist(lapply(st_geometry(mi_animals_tidy$geometry), 
                                                 "length")) == 1,]
mi_animals_tidy$area <- units::set_units(
  st_area(st_cast(mi_animals_tidy$geometry, "POLYGON")), "acres")
mi_animals_tidy <- mutate(mi_animals_tidy, animal_density = total / area)

# str_detect(short_desc, "IRRIGATED"))

ggplot() +
  geom_sf(data = mi_animals_tidy, aes(fill = total)) +
  labs(fill = "2007 USDA Census \n Total animals")

ggplot() +
  geom_sf(data = mi_animals_tidy, aes(fill = animal_density)) +
  labs(fill = "2007 USDA Census \n animal # per acre")

# CAFOS
# cattle
params <- list("source_desc" = "CENSUS",
               "commodity_desc"="CATTLE",
               "year" = 2007,
               "state_alpha" = "MI",
               "domain_des" = "PRODUCTION",
               "agg_level_desc" = "COUNTY",
               # "unit_desc" = "ACRES",
               key = key)
mi_animals     <- nassqs(params)

test <- dplyr::filter(mi_animals, 
                      stringr::str_detect(domaincat_desc, 
                      "SALES OF CATTLE ON FEED\\: \\(500 OR MORE HEAD\\)")) %>%
  dplyr::select(state_name, county_name, unit_desc, Value) %>%
  dplyr::arrange(state_name, county_name) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  tidyr::spread(unit_desc, Value) %>%
  mutate(cattle_density = HEAD / OPERATIONS) %>%
  mutate(has_cafo = tidyr::replace_na(cattle_density > 1000, FALSE))

# dairy
params <- list("source_desc" = "CENSUS",
               "commodity_desc"="CATTLE",
               "year" = 2007,
               "state_alpha" = "MI",
               "domain_desc" = "INVENTORY OF MILK COWS",
               "agg_level_desc" = "COUNTY",
               # "unit_desc" = "ACRES",
               key = key)
mi_animals     <- nassqs(params)

unique(mi_animals$domain_desc)

test <- dplyr::filter(mi_animals, 
                      stringr::str_detect(domaincat_desc, 
                                          "500 OR MORE HEAD"),
                      stringr::str_detect(short_desc, 
                                          "OPERATIONS")) %>%
  dplyr::select(state_name, county_name, unit_desc, Value) %>%
  dplyr::arrange(state_name, county_name) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  tidyr::spread(unit_desc, Value) %>%
  mutate(cattle_density = HEAD / OPERATIONS) %>%
  mutate(has_cafo = tidyr::replace_na(cattle_density > 1000, FALSE))
  
# broilers
params <- list("source_desc" = "CENSUS",
               "commodity_desc"="CHICKENS",
               "year" = 2007,
               "state_alpha" = "MI",
               "domain_desc" = "TOTAL",
               "agg_level_desc" = "COUNTY",
               "reference_period_desc" = "YEAR",
               # "unit_desc" = "ACRES",
               key = key)
mi_animals     <- nassqs(params)

test <- dplyr::filter(mi_animals, 
                      stringr::str_detect(short_desc, "BROILERS"), 
                      stringr::str_detect(prodn_practice_desc, "ALL")) %>%
  dplyr::select(state_name, county_name, unit_desc, Value) %>%
  dplyr::arrange(state_name, county_name) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  tidyr::spread(unit_desc, Value) %>%
  mutate(density = HEAD / OPERATIONS) %>%
  mutate(has_cafo = tidyr::replace_na(density > 50000, FALSE))

# hogs
params <- list("source_desc" = "CENSUS",
               "commodity_desc"="HOGS",
               "year" = 2007,
               "state_alpha" = "MI",
               "domain_desc" = "TOTAL",
               "agg_level_desc" = "COUNTY",
               "reference_period_desc" = "YEAR",
               # "unit_desc" = "ACRES",
               key = key)
mi_animals     <- nassqs(params)

test <- dplyr::filter(mi_animals, 
                      !stringr::str_detect(unit_desc, "\\$"), 
                      stringr::str_detect(prodn_practice_desc, "ALL")) %>%
  dplyr::select(state_name, county_name, unit_desc, Value) %>%
  dplyr::arrange(state_name, county_name) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  tidyr::spread(unit_desc, Value) %>%
  mutate(density = HEAD / OPERATIONS) %>%
  mutate(has_cafo = tidyr::replace_na(density > 2500, FALSE))
  
unique(test$domaincat_desc)

# tillage
params <- list("source_desc" = "CENSUS",
               "commodity_desc"="PRACTICES",
               "state_alpha" = "MI",
               "agg_level_desc" = "COUNTY",
               "domaincat_desc" = "AREA",
               # "unit_desc" = "ACRES",
               key = key)
mi_tillage     <- nassqs(params)

unique(mi_tillage$short_desc)
unique(mi_tillage$util_practice_desc)

test <- dplyr::filter(mi_animals, 
                      !stringr::str_detect(unit_desc, "\\$"), 
                      stringr::str_detect(prodn_practice_desc, "ALL")) %>%
  dplyr::select(state_name, county_name, unit_desc, Value) %>%
  dplyr::arrange(state_name, county_name) %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  tidyr::spread(unit_desc, Value) %>%
  mutate(density = HEAD / OPERATIONS) %>%
  mutate(has_cafo = tidyr::replace_na(density > 2500, FALSE))

unique(test$domaincat_desc)

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

# ---- plots ----

ggplot(data = ep) +
  geom_point(aes(x = iws_ag_2006_pcent, y = as.numeric(hu6_corn_pcent)))

plot_grid(
  ggplot(data = ep) +
    geom_point(aes(x = iws_ag_2006_pcent, y = tp)),
  ggplot(data = ep) +
    geom_point(aes(x = as.numeric(hu6_corn_pcent), y = tp))
)

# par(mfrow = c(1, 3))
# plot(ep$hu6_corn_pcent, ep$tp)
# plot(ep$iws_ag_2006_pcent, ep$tp)
# plot(ep$iws_ag_2006_pcent, ep$hu6_corn_pcent)
# abline(0, 1)
# par(mfrow = c(1,1))
