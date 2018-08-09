library(unpivotr)
library(tidyr)
library(tidyxl)
library(dplyr)
library(snakecase)
library(janitor)

# ---- usgs_get ----

# County-Level Estimates of Nutrient Inputs to the Land
# Surface of the Conterminous United States, 1982–2001

sfile <- "data/usgs_nutrient_inputs.xls"
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
sfile <- "data/usgs_nutrient_inputs.xls"
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

usgs <- left_join(county_info, datasheet, by = "row") %>%
  rename(value = character)

saveRDS(usgs, "data/usgs.rds")