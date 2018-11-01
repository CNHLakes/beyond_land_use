suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))

library(LAGOSNE)
library(LAGOSextra)
library(sf)
library(lwgeom)
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
states <- state_sf()[unlist(
  lapply(st_intersects(state_sf(), coordinatize(ep)), 
         function(x) length(x) > 0)),]$ABB

get_census <- function(state_alpha){
  source_desc = "CENSUS"
  year <- 2007
  agg_level_desc <- "COUNTY"
  census_key <- data.frame(id = c("cattle_total", "cattle_sales", 
                                  "cattle_dairy", "chicken_sales", "hog_sales"),
                           commodity = c("CATTLE", "CATTLE", 
                                         "CATTLE", "CHICKENS", "HOGS"), 
                           domain = c("TOTAL", "SALES OF CATTLE, INCL CALVES", 
                                      "INVENTORY OF MILK COWS", "TOTAL", "TOTAL"),
                           domaincat_desc = c(NA, "SALES OF CATTLE, INCL CALVES: (500 OR MORE HEAD)", 
                                              "INVENTORY OF MILK COWS: (500 OR MORE HEAD)", NA, NA),
                           unit_desc = c(NA, "OPERATIONS", "OPERATIONS", "OPERATIONS", "OPERATIONS"),
                           short_desc = c("CATTLE, INCL CALVES - INVENTORY", NA, NA, NA, NA),
                           ref_period_desc = c(NA, NA, NA, "YEAR", "YEAR"), 
                           freq_desc = c(NA, NA, NA, "ANNUAL", NA),
                           statisticcat_desc = c(NA, NA, NA, "SALES", "SALES"),
                           operation = c("identity", "identity", "identity", "sum", "identity"),
                           stringsAsFactors = FALSE)
  
  res <- apply(census_key, 1, function(x){
    # x <- census_key[5,]
    print(as.character(x["id"]))
    params <- list("source_desc" = source_desc, 
                   "commodity_desc" = as.character(x["commodity"]), 
                   "year" = year, 
                   "state_alpha" = state_alpha, 
                   "domain_desc" = as.character(x["domain"]), 
                   "domaincat_desc" = as.character(x["domaincat_desc"]),
                   "unit_desc" = as.character(x["unit_desc"]),
                   "short_desc" = as.character(x["short_desc"]), 
                   "reference_period_desc" = as.character(x["ref_period_desc"]),
                   "freq_desc" = as.character(x["freq_desc"]),
                   "agg_level_desc" = agg_level_desc, 
                   "statisticcat_desc" = as.character(x["statisticcat_desc"]),
                   key = key)
    params <- params[!is.na(params)]
    raw <- nassqs(params)
    raw$id <- as.character(x["id"])
    
    if(as.character(x["operation"]) == "sum"){
      raw <- dplyr::group_by(raw, county_name) %>%
        dplyr::summarize(Value = as.character(sum(as.numeric(Value, na.rm = TRUE)))) %>%
        left_join(dplyr::select(raw, -Value), by = "county_name") %>%
        distinct(county_name, .keep_all = TRUE)
    }
    
    raw
    })
  
  
  res <- dplyr::bind_rows(res) %>%
    group_by(state_name, county_name, year, id) %>%
    summarize(value = first(Value)) %>%
    tidyr::spread(id, value)
  res
}

# setwd("../")
if(!file.exists("data/census/census_raw.rds")){
  # unlink("data/census/census_raw.rds")
  # find states intersected by iws
  animals_raw <- lapply(states, function(x){
    # x <- "MN"
    print(x)
    raw <- get_census(x)
  })
  
  saveRDS(animals_raw, "data/census/census_raw.rds")
}

animals_raw <- readRDS("data/census/census_raw.rds")
animals_raw <- dplyr::bind_rows(animals_raw)
animals_raw <- mutate(animals_raw, state = tolower(state_name),
             county = gsub("\\.", "", gsub(" ", "", tolower(county_name))))
animals_raw <- mutate_at(animals_raw, vars(matches("cattle|chicken|hog")), funs(as.numeric))
animals_raw <- left_join(animals_raw, county_sf(), by = c("state", "county"))
st_geometry(animals_raw) <- animals_raw$geometry

# animals_raw$area <- units::set_units(
#   st_area(animals_raw$geometry), "acres")
# animals_raw <- mutate(animals_raw, animal_density = total / area)

# check area calcs
# units::set_units(
#   st_area(dplyr::filter(county_sf(), state == "wisconsin", county == "pepin")), "acres")
# units::set_units(
#   st_area(dplyr::filter(animals_raw, state == "wisconsin", county == "pepin")), "acres")  
# dplyr::filter(animals_raw, state == "wisconsin", county == "pepin")
# mapview::mapview(animals_raw, zcol = "animal_density")

# select counties intersected by ep iws
animals_raw <- st_transform(animals_raw, st_crs(iws))
animals     <- interp_to_iws(animals_raw, varname = "hog_sales", 
                             outname = "hog_sales", is_extensive = TRUE) 
# kg per ha is false, kg is true

saveRDS(animals_raw, "data/census/census.rds")

# ---- viz ----
# animals <- readRDS("data/census/census.rds")
# st_geometry(animals) <- NULL
# animals <- left_join(coordinatize(ep), animals, by = "lagoslakeid")
#
# mapview::mapview(animals, zcol = "hog_sales")

# ---- debug ----
# library(mapview)
# llid <- 885
# t_iws <- dplyr::filter(iws, lagoslakeid == llid)
# t_county <- county_sf[unlist(lapply(
#   st_intersects(county_sf, t_iws), function(x) length(x) > 0)), ]
# t_raw <- animals_raw[unlist(lapply(
#   st_intersects(animals_raw, t_iws), function(x) length(x) > 0)), ]
# 
# mapview(t_raw) +
#   mapview(t_iws)
# 
# units::set_units(
#   st_area(dplyr::filter(county_sf(), state == "wisconsin", county == "pepin")), "acres")  
