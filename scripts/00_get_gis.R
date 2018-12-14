#' 00_get_gis.R
#' =======================================================
#+ setup, include=FALSE
knitr::opts_chunk$set(eval = FALSE)
#+


# setwd("../")
source("scripts/99_utils.R")

# ---- size_comparison ----
lg   <- lagosne_load()
gpkg_path <- "data/gis.gpkg"

ep  <- readRDS("data/ep.rds") %>%
  st_as_sf(coords = c("nhd_long", "nhd_lat"), crs = 4326)

iws  <- LAGOSNEgis::query_gis("IWS", "lagoslakeid", ep$lagoslakeid)

state_codes <- c("IL", "IN", "IA",
                 "MI", "MN", "MO",
                 "NY", "OH", "PA", "WI")
states <- state_sf() %>%
  dplyr::filter(., ABB %in% state_codes)

counties <- dplyr::filter(county_sf(), state_abb %in% state_codes)
cnty_lg <- lg$county %>%
  mutate(county_name = gsub(" county", "", tolower(county_name))) %>%
  left_join(lg$state, by = c("county_state" = "state")) %>%
  mutate(county_name = gsub("\\.", "", gsub(" ", "", county_name))) %>%
  mutate(county_name = gsub("'", "", gsub("saint", "st", county_name))) %>%
  mutate(state_name = tolower(state_name)) %>%
  select(state_zoneid, state_name, county_name, county_state, county_zoneid) %>%
  # dplyr::filter(cnty_lg, str_detect(county_name, "brien"))
  left_join(st_drop_geometry(counties), .,
            by = c("state" = "state_name", "county" = "county_name")) %>%
  select(state_abb, county, county_zoneid)
counties <- left_join(counties, cnty_lg, by = c("state_abb", "county"))

# add zoneids

# use LAGOSNE to pull hu ids that correspond to states
# lg <- lagosne_load()
# pad states with NA to 10 characters
hu4_zones <- lg$hu4 %>%
  mutate(hu4_states = gsub("  ", "NA", sprintf("%-10s", hu4_states, "NA"))) %>%
  tidyr::separate(hu4_states, into = paste0("state_", 1:5), sep = seq(2, 8, 2)) %>%
  dplyr::select(starts_with("state"), hu4_zoneid, hu4) %>%
  tidyr::gather(key = "state", value = "value", -hu4_zoneid, -hu4) %>%
  dplyr::filter(value %in% state_codes) %>%
  distinct(hu4, hu4_zoneid)

hu4s <- LAGOSNEgis::query_gis("HU4", "ZoneID", hu4_zones$hu4_zoneid)
hu8s <- LAGOSNEgis::query_gis_(
  query = paste0("SELECT * FROM HU8 WHERE ",
                 paste0("HUC8 LIKE '", hu4s$HUC4, "%'", collapse = " OR ")))

# unlink("data/gis.gpkg")
# st_layers("data/gis.gpkg")
st_write(states, gpkg_path, layer = "states",
         layer_options = c("OVERWRITE=yes"))
st_write(hu4s, gpkg_path, layer = "hu4s", update = TRUE,
         layer_options = c("OVERWRITE=yes"))
st_write(hu8s, gpkg_path, layer = "hu8s", update = TRUE,
         layer_options = c("OVERWRITE=yes"))
st_write(counties, gpkg_path, layer = "counties", update = TRUE,
         layer_options = c("OVERWRITE=yes"))
st_write(iws, gpkg_path, layer = "iws", update = TRUE,
         layer_options = c("OVERWRITE=yes"))