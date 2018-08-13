library(LAGOSNE)
library(LAGOSextra)
library(sf)
library(unpivotr)
library(tidyr)
library(tidyxl)
library(dplyr)
library(snakecase)
library(janitor)
library(lwgeom)

# ---- usgs_get ----

# County-Level Estimates of Nutrient Inputs to the Land
# Surface of the Conterminous United States, 1982–2001

ep <- readRDS("data/ep.rds")
iws <- LAGOSextra::query_wbd(ep$lagoslakeid)
iws <- st_make_valid(iws)

sfile <- "data/usgs/usgs_nutrient_inputs.xls"
ofile <- paste0(sfile, "x")

if(!file.exists(ofile)){
  # convert xls to xlsx
  jsta::get_if_not_exists("https://pubs.usgs.gov/sir/2006/5012/excel/Nutrient_Inputs_1982-2001jan06.xls", sfile, overwrite = FALSE)
  writexl::write_xlsx(readxl::read_excel(sfile), ofile)

  # # file.exists(sfile)
  # print("Open the libreoffice gui!!!")
  # system(paste0("libreoffice --convert-to xlsx ", sfile, " --headless"))
  # system(paste0("mv ", basename(paste0(sfile, "x")), " ", paste0(sfile, "x")))
}

# setwd("../")
sfile <- "data/usgs/usgs_nutrient_inputs.xls"
ofile <- paste0(sfile, "x")

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

# saveRDS(usgs_raw, "data/usgs/usgs_raw.rds")

# select counties intersected by ep iws
county_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
county_sf <- tidyr::separate(county_sf, ID, c("state", "county"), sep = ",")
county_sf <- st_transform(county_sf, st_crs(iws))
county_sf <- st_make_valid(county_sf)
county_sf <- county_sf[
  unlist(lapply(
    st_intersects(county_sf, iws),
    function(x) length(x) > 0)),]

# join usgs data
# usgs_raw <- readRDS("data/usgs/usgs_raw.rds")
usgs <- filter(usgs_raw, stringr::str_detect(variable, "phosphorus")) %>%
  group_by(county, state) %>%
  summarize(value = sum(as.numeric(value), na.rm = TRUE))

state_key <- data.frame(state = datasets::state.abb, state_name = datasets::state.name)
usgs <- ungroup(usgs) %>%
  mutate(county = tolower(county)) %>%
  left_join(state_key) %>%
  mutate(state = tolower(state_name))

county_usgs <- left_join(county_sf, usgs)

# st_interpolate_aw
iws_interp <- st_interpolate_aw(county_usgs["value"], iws,
                                extensive = TRUE)
iws_interp <- data.frame(phosphorus_input = iws_interp$value,
                         lagoslakeid = iws$lagoslakeid,
                         stringsAsFactors = FALSE)

res <- left_join(ep, iws_interp, by = "lagoslakeid")

# ---- nitrogen ----
usgs <- filter(usgs_raw, stringr::str_detect(variable, "nitrogen")) %>%
  group_by(county, state) %>%
  summarize(value = sum(as.numeric(value), na.rm = TRUE))

state_key <- data.frame(state = datasets::state.abb, state_name = datasets::state.name)
usgs <- ungroup(usgs) %>%
  mutate(county = tolower(county)) %>%
  left_join(state_key) %>%
  mutate(state = tolower(state_name))

county_usgs <- left_join(county_sf, usgs)

iws_interp <- st_interpolate_aw(county_usgs["value"], iws,
                                extensive = TRUE)
iws_interp <- data.frame(nitrogen_input = iws_interp$value,
                         lagoslakeid = iws$lagoslakeid,
                         stringsAsFactors = FALSE)

res <- left_join(res, iws_interp, by = "lagoslakeid")

saveRDS(res, "data/usgs/usgs.rds")

# ---- viz ----
# res <- readRDS("data/usgs/usgs.rds")
# res$value <- log(res$value)
#
# mapview::mapview(county_sf) +
# mapview::mapview(coordinatize(res), zcol = "value")
