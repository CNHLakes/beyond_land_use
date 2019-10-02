source("scripts/99_utils.R")
# library(LAGOSNE)
# library(LAGOSNEgis)
# library(sf)
# library(unpivotr)
# suppressMessages(library(tidyr))
# library(tidyxl)
# suppressMessages(library(dplyr))
# library(snakecase)
# library(lwgeom)
# suppressMessages(library(magrittr))

# ---- usgs_get ----

# County-Level Estimates of Nutrient Inputs to the Land
# Surface of the Conterminous United States, 1982–2001

ep     <- readRDS("data/ep.rds")
states <- c("OH", "IL", "MI", "WI", "IN", "MN", "IA", "MO", "PA", "NY")
iws    <- LAGOSNEgis::query_wbd(ep$lagoslakeid, utm = FALSE)
# mapview::mapview(dplyr::filter(iws, lagoslakeid == 34352))
iws    <- st_make_valid(iws)

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
message("Parsing raw county data...")
if(!file.exists("data/usgs/usgs_raw.rds")){
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
    janitor::clean_names()

  usgs_raw <- left_join(county_info, datasheet, by = "row") %>%
    rename(value = character)

  saveRDS(usgs_raw, "data/usgs/usgs_raw.rds")
}

message("Interpolating county data to watersheds...")
# select counties intersected by ep iws
county_ll <- query_gis_(query = paste0("SELECT * FROM COUNTY WHERE ",
                          paste0("STATE LIKE '", states, "%'", collapse = " OR ")))
county_ll <- county_ll[
  unlist(lapply(
    st_intersects(county_ll, iws),
    function(x) length(x) > 0)),]
county_ll <- left_join(county_ll, 
dplyr::select(readRDS("data/county_lulc.rds"), 
              county_zoneid, county_ag_2011_pcent), 
by = c("ZoneID" = "county_zoneid"))
county_ll <- mutate(county_ll, 
                    ag_area = units::set_units(st_area(geometry), "ha") * county_ag_2011_pcent, 
                    total_area = units::set_units(st_area(geometry), "ha"))
county_ll <- mutate(county_ll, state = STATE)

interp_to_iws <- function(usgs_raw, varname, outname){
  # varname = "nitrogen_livestock_manure"
  message(paste0("Interpolating ", varname, " ..."))
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
    usgs <- mutate(key_state(mutate(usgs, state.name = state_name)), 
                   state = state.abb)

  # if !atmospheric deposition area used here is all (and only) Ag
  if(length(grep("deposition", varname)) > 0){
    county_usgs <- left_join(county_ll, usgs) %>%
      mutate(value_per_ha = value / total_area) %>% 
      dplyr::filter(!is.na(value)) # hist(county_usgs$value_per_ha)
  }else{
    county_usgs <- left_join(county_ll, usgs) %>%
      mutate(value_per_ha = value / ag_area) %>% 
      dplyr::filter(!is.na(value)) # hist(county_usgs$value_per_ha)
  }
  
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

message("Binding variables...")
usgs <- bind_cols(usgs) %>%
  dplyr::select(lagoslakeid, usgs_key$pretty_name)

message("Calculating total inputs...")
usgs <- usgs %>%
  mutate(n_input = rowSums(select(., starts_with("nitrogen")), na.rm = TRUE),
         p_input = rowSums(select(., starts_with("phosphorus")), na.rm = TRUE))

saveRDS(usgs, "data/usgs/usgs.rds")
# usgs_raw <- readRDS("data/usgs/usgs_raw.rds")
# usgs     <- readRDS("data/usgs/usgs.rds")

# ---- viz ----
# res_b <- readRDS("data/usgs/usgs_backup.rds")
# res_b$n_input <- log(res_b$n_input)
# 
# res <- readRDS("data/usgs/usgs.rds")
# res$n_input <- log(res$n_input)
# res <- dplyr::filter(res, lagoslakeid %in% res_b$lagoslakeid)
# 
# plot(res$n_input, res_b$n_input)
#
# mapview::mapview(county_sf) +
# mapview::mapview(coordinatize(res), zcol = "value")
