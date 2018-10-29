suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
library(snakecase)
library(janitor)
library(lwgeom)
suppressMessages(library(magrittr))

library(LAGOSNE)
library(LAGOSextra)
library(sf)
library(ggplot2)
library(rnassqs)
library(maps)
library(stringr)
library(concaveman)
source("scripts/utils.R")
key <- "E44D2FCF-E267-3DE1-950A-E6C54EEA7058"
Sys.setenv(NASSQS_TOKEN = key)

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
  
  animals_raw <- lapply(states, function(x){
    # x <- "MN"
    print(x)
    params <- list("source_desc" = "CENSUS",
                   "commodity_desc" = c("CATTLE"),
                   "year" = 2007,
                   "state_alpha" = x,
                   "domain_desc" = "TOTAL",
                   "agg_level_desc" = "COUNTY",
                   key = key)
    raw <- nassqs(params)
  
    # check unique short_desc field
    
    census_tidy <- mutate(raw, state = tolower(state_name),
             county = gsub("\\.", "", gsub(" ", "", tolower(county_name))),
             value = as.numeric(gsub(",", "", .data$Value))) %>%
      filter(!is.na(county) & nchar(county) > 0 & year %in% params$year) %>%
      dplyr::select(state, county, value, short_desc, domain_desc) %>%
      group_by(county) %>%
      mutate(total = sum(value, na.rm = TRUE)) %>%
      left_join(county_sf(), by = c("state", "county"))
    
    census_tidy$area <- units::set_units(
      st_area(st_cast(census_tidy$geometry, "POLYGON")), "acres")
    census_tidy <- mutate(census_tidy, animal_density = total / area)
    st_geometry(census_tidy) <- census_tidy$geometry
  
    # mapview::mapview(census_tidy, zcol = "total")
    census_tidy})
  
  animals_raw <- dplyr::bind_rows(animals_raw)

  saveRDS(animals_raw, "data/census/census_raw.rds")
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
