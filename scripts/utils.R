suppressMessages(library(sf))
suppressMessages(library(dplyr))

# ---- basic_gis ----
county_sf <- function(){
  county_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
  county_sf <- tidyr::separate(county_sf, ID, c("state", "county"), ",")
  county_sf$county <- gsub("\\.", "", gsub(" ", "", county_sf$county))
  county_sf
}

state_sf <- function(){
  state_sf <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
  key <- data.frame(ID = tolower(state.name),
                    ABB = state.abb, stringsAsFactors = FALSE)
  left_join(state_sf, key, by = "ID")
}

get_states <- function(bbox){
  state_sf <- sf::st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
  key <- data.frame(ID = tolower(state.name),
                    ABB = state.abb, stringsAsFactors = FALSE)
  state_sf <- left_join(state_sf, key, by = "ID")
  bbox <- st_transform(st_as_sfc(bbox), st_crs(state_sf))

  state_sf <- state_sf[unlist(lapply(
    st_intersects(state_sf, bbox),
    function(x) length(x) > 0)),]

  state_sf$ABB
}

get_bbox <- function(boundary_iws){
  st_bbox(boundary_iws)
}

clip_to_iws <- function(iws, r){
  raster::mask(r, as_Spatial(iws))
}

get_raster_name <- function(r, path){
  paste0(path, r@data@names)
}

get_iws <- function(lagoslakeid){
  LAGOSextra::query_wbd(lagoslakeid, utm = FALSE)
}
