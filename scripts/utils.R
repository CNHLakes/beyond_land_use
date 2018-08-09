library(sf)
library(dplyr)

# ---- county_sf ----

county_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
county_sf <- tidyr::separate(county_sf, ID, c("state", "county"))

# ---- state_sf ----

state_sf <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
key <- data.frame(ID = tolower(state.name), 
                  ABB = state.abb, stringsAsFactors = FALSE)
state_sf <- left_join(state_sf, key, by = "ID")

