library(cdlTools)
library(tidyr)
library(dplyr)
library(magrittr)

cdl_key <- data.frame(code = 0:255, description = cdlTools::updateNamesCDL(0:255),
                      stringsAsFactors = FALSE)
cdl_key <- cdl_key[which(nchar(cdl_key$description) > 3),]

aggregate_categories <- function(cdl_key){
  res <- mutate(cdl_key, category = case_when(
    grepl("wheat", tolower(description)) ~ "wheat",
    grepl("wetlands", tolower(description)) ~ "wetlands",
    grepl("developed", tolower(description)) ~ "developed",
    grepl("dbl", tolower(description)) ~ "mixed crop",
    grepl("background", tolower(description)) ~ "background",
    grepl("barren", tolower(description)) ~ "other non ag",
    grepl("corn", tolower(description)) ~ "corn",
    grepl("soybeans", tolower(description)) ~ "soybeans",
    grepl("alfalfa", tolower(description)) ~ "forage",
    grepl("pasture", tolower(description)) ~ "pasture",
    grepl("grassland", tolower(description)) ~ "other non ag",
    grepl("shrubland", tolower(description)) ~ "other non ag",
    grepl("forest", tolower(description)) ~ "forest",
    grepl("nonag", tolower(description)) ~ "other non ag",
    grepl("water$", tolower(description)) ~ "water",
    grepl("aqua", tolower(description)) ~ "other non ag",
    grepl("clouds", tolower(description)) ~ "other non ag"
  )) %>%
    tidyr::replace_na(list(category = "other ag"))

  res <- mutate(res,
                is_ag = if_else(category %in% c("corn", "wheat", "other ag",
                                          "soybeans", "mixed crop", "pasture",
                                          "forage"),
                                          "ag", "nonag"))
  res
}
cdl_key <- aggregate_categories(cdl_key)
write.csv(cdl_key, "data/cdl/cdl_key.csv", row.names = FALSE)