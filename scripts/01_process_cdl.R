library(raster)
library(cdlTools)
suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
library(progress)
cdl_path <- "data/cdl/"

cdl_key <- read.csv("data/cdl/cdl_key.csv", stringsAsFactors = FALSE)

r_list <- list.files(cdl_path, pattern = "^\\d*.tif$", 
                     include.dirs = TRUE, full.names = TRUE)

cdl_summary <- function(llid){
  # llid <- 4393
  r <- raster(r_list[grep(paste0(llid, ".tif"), r_list)[1]])
  
  cdl_table <- as_data_frame(table(values(r)), stringsAsFactors = FALSE) %>%
    mutate(code = as.integer(Var1)) %>%
    dplyr::select(-Var1) %>%
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
  
  cdl_isforage <- cdl_table %>%
    filter(description != "Background" & !is.na(description)) %>%
    arrange(desc(n)) %>%
    group_by(is_forage) %>%
    summarize(group_n = sum(n, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(percent = round((group_n / sum(group_n, na.rm = TRUE)) * 100, 0)) %>%
    filter(is_forage %in% c("forage", "pasture")) %>%
    arrange(percent)
  
  res <- mutate(cdl_cat, llid = llid, variable = category, value = percent) %>%
    select(llid, variable, value) %>%
    rbind(
      select(mutate(cdl_isag, llid = llid, variable = is_ag, value = percent), 
             llid, variable, value),
      select(mutate(cdl_isforage, llid = llid, variable = is_forage, value = percent), 
             llid, variable, value)
      )
  
  res[!duplicated(res$variable),]
}

pb <- progress_bar$new(format = "  pulling stats for :llid [:bar]", 
                       total = length(r_list), 
                       clear = FALSE)
res <- list()
for(i in seq_len(length(r_list))){
  # i <- 1
  llid <- as.numeric(stringr::str_extract(r_list, "\\d*(?=(.tif))"))[i]
  pb$tick(tokens = list(llid = llid))
  res[[i]] <- cdl_summary(llid)
}

res <- bind_rows(res)
res <- dplyr::filter(res, value >=0)
res <- tidyr::spread(res, variable, value, -llid, fill = NA)

write.csv(res, "data/cdl/cdl_summary.csv", row.names = FALSE)
