source("scripts/99_utils.R")

# response variables are medians from 1995 - 2005
# setwd("../")
ep          <- readRDS("data/ep.rds")

# predictors are at the iws level and are as close in time to 2000 as possible
lg_lulc     <- readRDS("data/iws_lulc.rds") %>%
  dplyr::select(-iws_ag_2011_pcent)

usgs        <- readRDS("data/usgs/usgs.rds")

gssurgo     <- ungroup(readRDS("data/gssurgo/gssurgo.rds")$res)
gssurgo_key <- readRDS("data/gssurgo/gssurgo.rds")$gssurgo_key

cdl         <- read.csv("data/cdl/cdl_summary.csv", stringsAsFactors = FALSE)

lake_buffer_lulc <- read.csv("data/buffer_lulc.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(llid, description, percent_lake) %>%
  dplyr::mutate(description = snakecase::to_any_case(description)) %>%
  tidyr::spread(description, percent_lake) %>%
  # preprend lake to col names 
  setNames(paste0("lake_", names(.))) %>%
  rename(llid = lake_llid) %>%
  # assert that lulc adds up to 100%
  mutate(lulc_sum = rowSums(
    dplyr::select(., 
                  lake_barren_land_rock_sand_clay:lake_woody_wetlands), 
    na.rm = TRUE)) %>% 
  assertr::assert(within_bounds(99.999, 100.001), lulc_sum) %>%
  dplyr::select(-lulc_sum)

stream_buffer_lulc <- read.csv("data/buffer_lulc.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(llid, description, percent_stream) %>%
  dplyr::mutate(description = snakecase::to_any_case(description)) %>%
  tidyr::spread(description, percent_stream) %>%
  # preprend stream to col names 
  setNames(paste0("stream_", names(.))) %>%
  rename(llid = stream_llid) %>%
  # assert that lulc adds up to 100%
  mutate(lulc_sum = rowSums(
    dplyr::select(., 
                  stream_barren_land_rock_sand_clay:stream_woody_wetlands), 
    na.rm = TRUE)) %>%
  mutate(lulc_sum = case_when(
    lulc_sum == 0 ~ NA_real_,
    lulc_sum == 100 ~ 100, 
    TRUE ~ NA_real_)) %>%
  assertr::assert(in_set(NA, 100), lulc_sum) %>%
  dplyr::select(-lulc_sum)

# ---- collect_response_variables ----
dt <- ep %>%
  left_join(lg_lulc, by = "lagoslakeid") %>%
  left_join(usgs, by = "lagoslakeid") %>%
  left_join(mutate(gssurgo, llid = as.integer(as.character(llid))), 
            by = c("lagoslakeid" = "llid")) %>%
  left_join(cdl, by = c("lagoslakeid" = "llid")) %>%
  left_join(lake_buffer_lulc, by = c("lagoslakeid" = "llid")) %>%
  left_join(stream_buffer_lulc, by = c("lagoslakeid" = "llid"))

saveRDS(dt, "data/dt.rds")
write.csv(dt, "data/dt.csv", row.names = FALSE)
# dt <- readRDS("../data/dt.rds")

dt_scaled <- dt %>% 
  dplyr::select("lagoslakeid", "hu4_zoneid", "hu12_zoneid", 
                "tp", "tn", "hu12_ppt_mean", "hu12_baseflow_mean",
                "maxdepth", "iwsla_ratio", "ag", "row_crop_pct", "corn", 
                "soybeans", "pasture", "soil_org_carbon",
                "nitrogen_atmospheric_deposition", "clay_pct", 
                "lake_area_ha", "wetland_potential", contains("manure"), 
                contains("fertilizer"), contains("input"), 
                contains("cultivated_crops")) %>% 
  dplyr::filter(!is.na(phosphorus_fertilizer_use), 
                !is.na(soybeans), 
                !is.na(corn),
                !is.na(maxdepth), 
                !is.na(tp), 
                !is.na(tn)) %>%
  mutate_at(log, .vars = vars(tp, tn)) %>%
  mutate_at(scale, .vars = vars(-lagoslakeid, -hu4_zoneid, -hu12_zoneid))

saveRDS(dt_scaled, "data/dt_scaled.rds")
write.csv(dt_scaled, "data/dt_scaled.csv", row.names = FALSE)
# dt_scaled <- readRDS("data/dt_scaled.rds")

dt_units <- data.frame(variable = names(dt), stringsAsFactors = FALSE) %>%
  mutate(units = case_when(
    variable %in% names(cdl)[-1] ~ "percent",
    variable %in% names(usgs)[-1] ~ "kg/ha",
    grepl("_ha$", tolower(variable)) ~ "hectares",
    grepl("_ratio$", tolower(variable)) ~ "",
    variable %in% names(ep)[c(-1, -2, -3)] ~ "ug/l",
    grepl("pct$", tolower(variable)) ~ "percent"
  ))

dt_units <- left_join(dt_units, 
          dplyr::select(gssurgo_key, metric, units = agg_type), 
          by = c("variable" = "metric")) %>%
  mutate(units = coalesce(units.x, units.y)) %>%
  dplyr::select(variable, units)

saveRDS(dt_units, "data/dt_units.rds")
