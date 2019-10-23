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
  rowwise() %>%
  # calculate ag, forest, developed, and "natural"
  mutate(ag = sum(cultivated_crops, pasture_hay, na.rm = TRUE), 
         forest = sum(deciduous_forest, evergreen_forest, mixed_forest, na.rm = TRUE), 
         developed = sum(developed_low_intensity, developed_medium_intensity, 
                         developed_high_intensity, developed_open_space, na.rm = TRUE), 
         natural = sum(forest, grassland_herbaceous, barren_land_rock_sand_clay, emergent_herbaceous_wetlands, scrub_shrub, woody_wetlands, na.rm = TRUE)) %>%
  ungroup() %>%
  # preprend lake to col names 
  setNames(paste0("lake_", names(.))) %>%
  rename(llid = lake_llid) %>%
  # assert that lulc adds up to 100%
  mutate(lulc_sum = rowSums(
    dplyr::select(., 
                  lake_barren_land_rock_sand_clay:lake_woody_wetlands), 
    na.rm = TRUE)) %>% 
  assertr::assert(within_bounds(99.999, 100.001), lulc_sum) %>%
  dplyr::select(-lulc_sum) %>%
  dplyr::select(llid, lake_ag, lake_forest, lake_developed, lake_natural, 
                lake_cultivated_crops)

stream_buffer_lulc <- read.csv("data/buffer_lulc.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(llid, description, percent_stream) %>%
  dplyr::mutate(description = snakecase::to_any_case(description)) %>%
  tidyr::spread(description, percent_stream) %>%
  # calculate ag, forest, developed, and "natural"
  rowwise() %>%
  mutate(ag = sum(cultivated_crops, pasture_hay, na.rm = TRUE), 
         forest = sum(deciduous_forest, evergreen_forest, mixed_forest, na.rm = TRUE), 
         developed = sum(developed_low_intensity, developed_medium_intensity, 
                         developed_high_intensity, developed_open_space, na.rm = TRUE), 
         natural = sum(forest, grassland_herbaceous, barren_land_rock_sand_clay, emergent_herbaceous_wetlands, scrub_shrub, woody_wetlands, na.rm = TRUE)) %>%
  ungroup() %>%
  setNames(paste0("stream_", names(.))) %>% # preprend stream to col names 
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
  dplyr::select(-lulc_sum) %>%
  dplyr::select(llid, stream_ag, stream_forest, stream_developed, stream_natural, 
                stream_cultivated_crops)

# create object holding lake buffer lulc if no stream buffer info
missing_stream_lulc <- stream_buffer_lulc %>% 
  dplyr::select(-llid) %>%
  apply(1, function(x) all(is.na(x) | x == 0)) %>%
  cbind(stream_buffer_lulc$llid) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(c("no_streams", "llid")) %>%
  mutate(no_streams = as.logical(no_streams))

buffer_lulc <- bind_rows(
  setNames(dplyr::filter(stream_buffer_lulc, 
                llid %in% dplyr::filter(missing_stream_lulc, !no_streams)$llid), 
           c("llid", "ag", "forest", "developed", "natural", "cultivated_crops")),
  setNames(dplyr::filter(lake_buffer_lulc, 
                llid %in% dplyr::filter(missing_stream_lulc, no_streams)$llid), 
           c("llid", "ag", "forest", "developed", "natural", "cultivated_crops"))
  ) %>% arrange(llid) %>% 
  # left_join(missing_stream_lulc, by = "llid") %>%
  setNames(paste0("buffer_", names(.))) %>%
  rename(llid = buffer_llid)

# ---- collect_response_variables ----
dt <- ep %>%
  left_join(lg_lulc, by = "lagoslakeid") %>%
  left_join(usgs, by = "lagoslakeid") %>%
  left_join(mutate(gssurgo, llid = as.integer(as.character(llid))), 
            by = c("lagoslakeid" = "llid")) %>%
  left_join(cdl, by = c("lagoslakeid" = "llid")) %>%
  left_join(lake_buffer_lulc, by = c("lagoslakeid" = "llid")) %>%
  left_join(stream_buffer_lulc, by = c("lagoslakeid" = "llid")) %>%
  left_join(buffer_lulc, by = c("lagoslakeid" = "llid"))

saveRDS(dt, "data/dt.rds")
write.csv(dt, "data/dt.csv", row.names = FALSE)
# setwd("tables")
# dt <- readRDS("../data/dt.rds")

dt_units <- data.frame(variable = names(dt), stringsAsFactors = FALSE) %>%
  mutate(units = case_when(
    variable %in% names(cdl)[-1] ~ "percent",
    variable %in% names(usgs)[-1] ~ "kg/ha",
    grepl("_ha$", tolower(variable)) ~ "hectares",
    grepl("_ratio$", tolower(variable)) ~ "",
    grepl("deposition", tolower(variable)) ~ "kg/ha",
    grepl("baseflow", tolower(variable)) ~ "",
    grepl("_ppt_", tolower(variable)) ~ "mm/yr",
    grepl("pct$", tolower(variable)) ~ "percent",
    grepl("buffer", tolower(variable)) ~ "percent",
    variable %in% "maxdepth" ~ "m",
    variable %in% "soil_org_carbon" ~ "g C/m2",
    # keep below line as last
    variable %in% names(ep)[c(-1, -2, -3, -9, -10, -12)] ~ "ug/l"
  ))

dt_units <- left_join(dt_units, 
                      dplyr::select(gssurgo_key, metric, units = agg_type), 
                      by = c("variable" = "metric")) %>%
  mutate(units = coalesce(units.x, units.y)) %>%
  dplyr::select(variable, units)

saveRDS(dt_units, "data/dt_units.rds")

# ---- create scaled version of predictors for modelling ----

cdl_vars <- dt %>%
  dplyr::select(ag:wheat, -natural, -nonnatural) %>%
  summarize_all(median, na.rm = TRUE) %>%
  tidyr::gather() %>%
  dplyr::filter(value >= 1.4) %>%
  arrange(desc(value)) %>%
  pull(key)

dt_scaled <- dt %>% 
  dplyr::select("lagoslakeid", "hu4_zoneid", "hu12_zoneid", 
                "tp", "tn", "chla", "hu12_ppt_mean", "hu12_baseflow_mean", 
                "hu4_nitrogen_atmospheric_deposition", "hu4_clay_pct",
                "maxdepth", "iwsla_ratio", "row_crop_pct", "soil_org_carbon",
                "nitrogen_atmospheric_deposition", "clay_pct", 
                "lake_area_ha", "wetland_potential", contains("manure"), 
                contains("fertilizer"), contains("input"), 
                contains("cultivated_crops"), 
                "stream_natural", "buffer_natural", cdl_vars) %>% 
  dplyr::filter(!is.na(phosphorus_fertilizer_use), 
                !is.na(wetlands),
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
