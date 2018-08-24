library(raster)
library(cdlTools)
library(magrittr)
library(dplyr)
cdl_path <- "data/cdl/"

cdl_key <- read.csv("data/cdl/cdl_key.csv", stringsAsFactors = FALSE)

r_list <- list.files(cdl_path, pattern = "^\\d*.tif$", 
                     include.dirs = TRUE, full.names = TRUE)

cdl_summary <- function(llid){
  # llid <- 126
  r <- raster(r_list[grep(llid, r_list)[1]])
  
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
  llid <- as.numeric(stringr::str_extract(r_list, "\\d*(?=(.tif))"))[i]
  print(i); print(llid)
  res[[i]] <- cdl_summary(llid)
}

res <- bind_rows(res)
write.csv(res, "data/cdl/cdl_summary.csv", row.names = FALSE)
