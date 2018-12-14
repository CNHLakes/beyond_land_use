suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))

library(LAGOSNE)
library(LAGOSNEgis)
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
iws <- LAGOSNEgis::query_wbd(ep$lagoslakeid, utm = FALSE)
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
animals_tidy <- dplyr::bind_rows(animals_raw)
animals_tidy <- mutate(animals_tidy, state = tolower(state_name),
             county = gsub("\\.", "", gsub(" ", "", tolower(county_name))))
clean_numeric <- function(x){
  x <- gsub(",", "", x)
  as.numeric(x)
}
animals_tidy <- mutate_at(animals_tidy, vars(matches("cattle|chicken|hog")), 
                          funs(clean_numeric))
# animals_tidy <- mutate_at(animals_tidy, vars(matches("cattle|chicken|hog")), 
#                           funs(tidyr::replace_na(., 0)))
animals_tidy <- left_join(animals_tidy, county_sf(), 
                          by = c("state", "county"))
st_geometry(animals_tidy) <- animals_tidy$geometry
saveRDS(animals_tidy, "data/census/census_tidy.rds")

# test <- dplyr::filter(animals_raw, state == "minnesota")
# test2 <- dplyr::filter(iws, lagoslakeid == 401)
# mapview::mapview(test, zcol = "hog_sales") +
#   mapview::mapview(test2) +
#   mapview::mapview(coordinatize(ep)) 

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
# setwd("../")
animals_tidy <- readRDS("data/census/census_tidy.rds")
animals_tidy <- st_transform(animals_tidy, st_crs(iws))
animals      <- lapply(tidyselect::vars_select(
  names(animals_tidy), matches("cattle|chicken|hog")), 
  function(x) interp_to_iws(animals_tidy, varname = x, outname = x, 
                            is_extensive = TRUE))
animals <- lapply(animals, function(x){ 
  st_geometry(x) <- NULL
  x
  })


animals <- purrr::reduce(animals, full_join, by = "lagoslakeid") %>%
  left_join(iws, by = "lagoslakeid")
st_geometry(animals) <- animals$geometry

saveRDS(animals, "data/census/census.rds")
