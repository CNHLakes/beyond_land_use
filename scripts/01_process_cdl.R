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
  
  cdl_table <- as.data.frame(table(values(r)), stringsAsFactors = FALSE) %>%
    mutate(code = as.integer(Var1)) %>%
    dplyr::select(-Var1) %>%
    left_join(cdl_key, by = "code") 
  
  cdl_cat <- cdl_table %>%
    dplyr::filter(description != "Background") %>%
    rename(n = Freq) %>%
    arrange(desc(n)) %>%
    group_by(category) %>%
    summarize(group_n = sum(n, na.rm = TRUE), 
              is_ag = first(is_ag), 
              is_natural = first(is_natural), 
              is_nfixer = first(is_nfixer), 
              is_forage = first(is_forage)) %>%
    ungroup() %>%
    mutate(percent = (group_n / sum(group_n, na.rm = TRUE)) * 100) %>%
    # dplyr::filter(percent >= 1) %>%
    arrange(is_ag, desc(percent))
  # cdl_cat <- add_row(cdl_cat, category = "other", group_n = NA,
  #                     percent = 100 - sum(cdl_cat$percent, na.rm = TRUE))
  category_order <- arrange(cdl_cat, is_ag, percent)$category
  
  calc_cdl_percent <- function(dt, grouping_column){
    dt %>%
      dplyr::filter(description != "Background") %>%
      rename(n = Freq) %>%
      arrange(desc(n)) %>%
      group_by(UQ(rlang::sym(grouping_column))) %>%
      summarize(group_n = sum(n, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(percent = (group_n / sum(group_n, na.rm = TRUE)) * 100) %>%
      arrange(UQ(rlang::sym(grouping_column)), desc(percent))
  }
  
  cdl_isag         <- calc_cdl_percent(cdl_table, "is_ag")
  cdl_isforage     <- calc_cdl_percent(cdl_table, "is_forage")
  cdl_isnatural    <- calc_cdl_percent(cdl_table, "is_natural")
  cdl_issmallgrain <- calc_cdl_percent(cdl_table, "is_small_grain")
  cdl_isnfixer     <- calc_cdl_percent(cdl_table, "is_nfixer")
    
  
  
  res <- mutate(cdl_cat, llid = llid, variable = category, value = percent) %>%
    dplyr::select(llid, variable, value) %>%
    rbind(
      dplyr::select(mutate(cdl_isag, llid = llid, variable = is_ag, 
                           value = percent), 
                    llid, variable, value),
      dplyr::select(mutate(cdl_isforage, llid = llid, variable = is_forage, 
                           value = percent), 
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
