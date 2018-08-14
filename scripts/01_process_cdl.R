library(raster)
library(cdlTools)
library(magrittr)
library(dplyr)
cdl_path <- "data/cdl/"
llid <- 100479

# cdl_key <- data.frame(code = 0:255, description = cdlTools::updateNamesCDL(0:255),
#                       stringsAsFactors = FALSE)
# cdl_key <- cdl_key[which(nchar(cdl_key$description) > 3),]
# aggregate_categories <- function(cdl_key){
#   res <- mutate(cdl_key, category = case_when(
#     grepl("wheat", tolower(description)) ~ "wheat",
#     grepl("wetlands", tolower(description)) ~ "wetlands",
#     grepl("developed", tolower(description)) ~ "developed",
#     grepl("dbl", tolower(description)) ~ "mixed crop",
#     grepl("background", tolower(description)) ~ "background",
#     grepl("barren", tolower(description)) ~ "other non ag",
#     grepl("corn", tolower(description)) ~ "corn",
#     grepl("soybeans", tolower(description)) ~ "soybeans",
#     grepl("alfalfa", tolower(description)) ~ "alfalfa",
#     grepl("pasture", tolower(description)) ~ "pasture",
#     grepl("grassland", tolower(description)) ~ "other non ag",
#     grepl("shrubland", tolower(description)) ~ "other non ag",
#     grepl("forest", tolower(description)) ~ "forest",
#     grepl("nonag", tolower(description)) ~ "other non ag",
#     grepl("water", tolower(description)) ~ "water",
#     grepl("aqua", tolower(description)) ~ "other non ag",
#     grepl("ice", tolower(description)) ~ "other non ag",
#     grepl("clouds", tolower(description)) ~ "other non ag"
#   )) %>%
#     tidyr::replace_na(list(category = "other ag"))
#   
#   res <- mutate(res, 
#                 is_ag = if_else(category %in% c("corn", "wheat", "other ag",
#                                           "soybeans", "mixed crop", "pasture",
#                                           "alfalfa"),
#                                           "ag", "nonag"))
#   res
# }
# cdl_key <- aggregate_categories(cdl_key)
# write.csv(cdl_key, "data/cdl/cdl_key.csv", row.names = FALSE)

cdl_key <- read.csv("data/cdl/cdl_key.csv", stringsAsFactors = FALSE)

r_list <- list.files(cdl_path, pattern = "^\\d*.tif$", 
                     include.dirs = TRUE, full.names = TRUE)

cdl_summary <- function(llid){
  r <- raster(r_list[grep(llid, r_list)])
  
  cdl_table <- as_data_frame(table(values(r)), stringsAsFactors = FALSE) %>%
    mutate(code = as.integer(Var1)) %>%
    select(-Var1) %>%
    left_join(cdl_key, by = "code") 
  
  cdl_cat <- cdl_table %>%
    filter(description != "Background") %>%
    arrange(desc(n)) %>%
    group_by(category) %>%
    summarize(group_n = sum(n, na.rm = TRUE), is_ag = first(is_ag)) %>%
    ungroup() %>%
    mutate(percent = round((group_n / sum(group_n, na.rm = TRUE)) * 100, 0)) %>%
    filter(percent >= 1) %>%
    arrange(is_ag, percent)
  
  cdl_cat <- add_row(cdl_cat, category = "other", group_n = NA,
                      percent = 100 - sum(cdl_cat$percent, na.rm = TRUE))
  category_order <- arrange(cdl_cat, is_ag, percent)$category
    
  cdl_isag <- cdl_table %>%
    filter(description != "Background") %>%
    arrange(desc(n)) %>%
    group_by(is_ag) %>%
    summarize(group_n = sum(n, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(percent = round((group_n / sum(group_n, na.rm = TRUE)) * 100, 0)) %>%
    arrange(percent)
  
  mutate(cdl_cat, llid = llid, variable = category, value = percent) %>%
    select(llid, variable, value) %>%
    rbind(mutate(cdl_isag, llid = llid, variable = is_ag, value = percent) %>%
            select(llid, variable, value))
}

res <- list()
for(i in seq_len(length(r_list))){
  llid <- as.numeric(stringr::str_extract(r_list[1], "\\d*(?=(.tif))"))
  res[[i]] <- cdl_summary(llid)
}
