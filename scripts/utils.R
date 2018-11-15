suppressMessages(library(sf))
suppressMessages(library(dplyr))
library(lwgeom)

# ---- basic_gis ----
county_sf <- function(){
  county_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
  county_sf <- tidyr::separate(county_sf, ID, c("state", "county"), ",")
  county_sf$county <- gsub("\\.", "", gsub(" ", "", county_sf$county))
  county_sf <- st_make_valid(county_sf)
  # county_sf <- county_sf[
  #   unlist(lapply(
  #     st_intersects(county_sf, iws),
  #     function(x) length(x) > 0)),]
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

state_key <- function(){
  data.frame(state = datasets::state.abb, 
             state_name = datasets::state.name)
}

# ---- advanced_gis ----

# kg per ha is false, kg is true
interp_to_iws <- function(raw, varname = "total", outname, is_extensive = TRUE){
  # raw <- animals_tidy
  # varname = "hog_sales"
  # is_extensive = TRUE
  print(varname)
  
  # raw <- dplyr::filter(animals_raw, county_name %in% c("LYON", "MURRAY", "PIPESTONE", "LINCOLN", "REDWOOD", "YELLOW MEDICINE") & state == "minnesota")
  
  raw <- raw[!is.na(raw[,varname])[,1],]
  raw <- raw[
      unlist(lapply(st_intersects(raw, iws), function(x) length(x) > 0)),]
  iws_sub <- iws[sapply(st_intersects(iws, raw), length) != 0,]
  # mapview::mapview(raw, zcol = "hog_sales") +
  #   mapview::mapview(iws)
  # median(raw$hog_sales)
  
  # st_interpolate_aw
  iws_interp <- suppressWarnings(
    st_interpolate_aw(raw[,varname], iws_sub,
                      extensive = is_extensive)) # kg per ha is false, kg is true
  iws_interp <- data.frame(outname = iws_interp[,varname],
                           lagoslakeid = iws_sub$lagoslakeid,
                           stringsAsFactors = FALSE)
  
  names(iws_interp)[1] <- outname
  iws_interp <- dplyr::select(iws_interp, lagoslakeid, everything())
  iws_interp <- dplyr::rename(iws_interp, geometry = outname.geometry)
  st_as_sf(iws_interp)
}
