library(cdlTools)
library(tidyr)
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))

cdl_key <- data.frame(code = 0:255, description = cdlTools::updateNamesCDL(0:255),
                      stringsAsFactors = FALSE)
cdl_key <- cdl_key[which(nchar(cdl_key$description) > 3),]

aggregate_categories <- function(cdl_key){
  
  res <- mutate(cdl_key, category = case_when(
    grepl("^alfalfa$", tolower(description)) ~ "forage",
    grepl("clover", tolower(description)) ~ "forage",
    grepl("^sorghum$", tolower(description)) ~ "sorghum",
    grepl("wheat", tolower(description)) ~ "wheat",
    grepl("wetlands", tolower(description)) ~ "wetlands",
    grepl("developed", tolower(description)) ~ "developed",
    grepl("dbl", tolower(description)) ~ "other ag",
    grepl("background", tolower(description)) ~ "background",
    grepl("barren", tolower(description)) ~ "other non ag",
    grepl("corn", tolower(description)) ~ "corn",
    grepl("soybeans", tolower(description)) ~ "soybeans",
    grepl("grass/pasture", tolower(description)) ~ "pasture",
    grepl("grassland", tolower(description)) ~ "other non ag",
    grepl("shrubland", tolower(description)) ~ "other non ag",
    grepl("forest", tolower(description)) ~ "forest",
    grepl("snow", tolower(description)) ~ "other non ag",
    grepl("nonag", tolower(description)) ~ "other non ag",
    grepl("water$", tolower(description)) ~ "water",
    grepl("aqua", tolower(description)) ~ "other non ag",
    grepl("clouds", tolower(description)) ~ "other non ag",
    grepl("other", tolower(description)) ~ "other ag"
  )) %>%
    tidyr::replace_na(list(category = "other ag"))
  
  res <- mutate(res, is_ag = case_when(
    grepl("^corn$", tolower(category)) ~ "ag",
    grepl("^sorghum$", tolower(category)) ~ "ag",
    grepl("^wheat$", tolower(category)) ~ "ag",
    grepl("^soybeans$", tolower(category)) ~ "ag",
    grepl("^other ag$", tolower(category)) ~ "ag",
    grepl("^forage$", tolower(category)) ~ "ag",
    grepl("^pasture$", tolower(category)) ~ "ag"
    )) %>%
    tidyr::replace_na(list(is_ag = "nonag"))

  res <- mutate(res, is_natural = case_when( 
    grepl("^ag$", tolower(is_ag)) ~ "nonnatural",
    grepl("developed", tolower(description)) ~ "nonnatural"
  )) %>%
    tidyr::replace_na(list(is_natural = "natural"))
  
  res <- mutate(res, is_small_grain = case_when(
    grepl("^barley$", tolower(description)) ~ "smallgrain",
    grepl("^sorghum$", tolower(description)) ~ "smallgrain",
    grepl("wheat", tolower(description)) ~ "smallgrain"
  ))
  
  res <- mutate(res, is_nfixer = case_when(
    grepl("^alfalfa$", tolower(description)) ~ "nfixer",
    grepl("soybeans", tolower(description)) ~ "nfixer"
  ))
  
  res <- mutate(res, is_forage = case_when(
    grepl("^pasture$", tolower(category)) ~ "forage",
    grepl("^forage$", tolower(category)) ~ "forage"
  ))
                  
  res
}
cdl_key <- aggregate_categories(cdl_key)
cdl_key <- dplyr::arrange(cdl_key, is_ag, code)

# View(cdl_key)
write.csv(cdl_key, "data/cdl/cdl_key.csv", row.names = FALSE)