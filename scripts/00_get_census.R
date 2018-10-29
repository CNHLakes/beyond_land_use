library(LAGOSNE)
library(LAGOSextra)
library(sf)
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
library(snakecase)
library(janitor)
library(lwgeom)
suppressMessages(library(magrittr))

library(ggplot2)
library(rnassqs)
library(maps)
library(stringr)
library(concaveman)

source("scripts/utils.R")

# ---- census_get ----

ep  <- readRDS("data/ep.rds")
iws <- LAGOSextra::query_wbd(ep$lagoslakeid, utm = FALSE)
# mapview::mapview(dplyr::filter(iws, lagoslakeid == 34352))
iws <- st_make_valid(iws)

# setwd("../")
if(!file.exists("data/census/census_raw.rds")){
  
  # find states intersected by iws
  states <- state_sf()[unlist(
    lapply(st_intersects(state_sf(), coordinatize(ep)), 
           function(x) length(x) > 0)),]$ABB
  
  params <- list("source_desc" = "CENSUS",
                 "commodity_desc"="CATTLE",
                 "year" = 2007,
                 "state_alpha" = "MI",
                 "domain_des" = "TOTAL",
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
  
  
  datasheet <- xlsx_cells(ofile) %>%
    select(row, col, data_type, numeric, character) %>%
    filter(col > 4) %>%
    mutate(character = if_else(grepl("X_", character), NA_character_, character),
           character = if_else(nchar(character) == 0, NA_character_, character)) %>%
    behead("N", variable) %>%
    behead("N", year) %>%
    behead("N", farm_nofarm) %>%
    fill(variable, year) %>%
    mutate(character = coalesce(character, as.character(numeric))) %>%
    select(-col, -numeric, -data_type) %>%
    mutate(variable = snakecase::to_any_case(variable),
           variable = gsub("_input_from", "", variable),
           variable = gsub("_kilograms", "", variable))

  county_info <- xlsx_cells(ofile) %>%
    select(row, col, data_type, numeric, character) %>%
    filter(col <= 4, col ) %>%
    behead("N", key) %>%
    filter(row > 4) %>%
    mutate(character = coalesce(character, as.character(numeric))) %>%
    select(row, value = character, key) %>%
    spread(key, value) %>%
    clean_names()

  usgs_raw <- left_join(county_info, datasheet, by = "row") %>%
    rename(value = character)

  saveRDS(usgs_raw, "data/usgs/usgs_raw.rds")
}

# select counties intersected by ep iws
county_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
county_sf <- tidyr::separate(county_sf, ID, c("state", "county"), sep = ",")
county_sf <- st_transform(county_sf, st_crs(iws))
county_sf <- st_make_valid(county_sf)
county_sf <- county_sf[
  unlist(lapply(
    st_intersects(county_sf, iws),
    function(x) length(x) > 0)),]

interp_to_iws <- function(usgs_raw, varname, outname){
  # varname = "nitrogen_livestock_manure"
  usgs <- filter(usgs_raw, stringr::str_detect(variable, varname)) %>%
    mutate(value = as.numeric(value)) %>%
    group_by(county, state, year, variable) %>%
    summarize(value = sum(as.numeric(value), na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(county, state, year) %>%
    summarize(value = sum(as.numeric(value), na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(county, state) %>%
    summarize(value = mean(as.numeric(value), na.rm = TRUE))

  state_key <- data.frame(state = datasets::state.abb,
                          state_name = datasets::state.name)

    usgs <- ungroup(usgs) %>%
              mutate(county = tolower(county)) %>%
              left_join(state_key) %>%
              mutate(state = tolower(state_name))

  county_usgs <- left_join(county_sf, usgs) %>%
    mutate(value_per_ha = value / units::set_units(st_area(geometry), "ha")) %>% 
    dplyr::filter(!is.na(value)) # hist(county_usgs$value_per_ha)
  
  iws <- iws[sapply(st_intersects(iws, county_usgs), length) != 0,]
  
  # st_interpolate_aw
  iws_interp <- suppressWarnings(
    st_interpolate_aw(county_usgs["value_per_ha"], iws,
                                  extensive = FALSE))
  # kg per ha is false, kg is true
  
  iws_interp <- data.frame(outname = iws_interp$value_per_ha,
                           lagoslakeid = iws$lagoslakeid,
                           stringsAsFactors = FALSE)
  names(iws_interp)[1] <- outname

  # iws_interp <- left_join(dplyr::select(ep, lagoslakeid, iws_ha), 
  #                         iws_interp, by = "lagoslakeid")
  
  dplyr::select(iws_interp, lagoslakeid, everything())
}


usgs_raw <- readRDS("data/usgs/usgs_raw.rds")

# unique(usgs_raw$variable)

usgs_key <- data.frame(variable = unique(usgs_raw$variable), 
                       pretty_name = unique(usgs_raw$variable), 
                       stringsAsFactors = FALSE)

usgs <- apply(usgs_key, 1, function(x) interp_to_iws(usgs_raw, x[1], x[2]))

usgs <- bind_cols(usgs) %>%
  dplyr::select(lagoslakeid, usgs_key$pretty_name)

usgs <- usgs %>% 
  mutate(n_input = rowSums(select(., starts_with("nitrogen")), na.rm = TRUE),
         p_input = rowSums(select(., starts_with("phosphorus")), na.rm = TRUE))

saveRDS(usgs, "data/usgs/usgs.rds")
# usgs <- readRDS("data/usgs/usgs.rds")

# ---- viz ----
# res <- readRDS("data/usgs/usgs.rds")
# res$value <- log(res$value)
#
# mapview::mapview(county_sf) +
# mapview::mapview(coordinatize(res), zcol = "value")
