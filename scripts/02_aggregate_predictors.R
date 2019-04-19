suppressMessages(library(dplyr))
suppressMessages(library(magrittr))

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
  mutate(description = janitor::make_clean_names(description))

# TODO: why is janitor butchering the names conversion?

test <- tidyr::spread(lake_buffer_lulc, description, percent_lake)

dt <- ep %>%
  left_join(lg_lulc, by = "lagoslakeid") %>%
  left_join(usgs, by = "lagoslakeid") %>%
  left_join(mutate(gssurgo, llid = as.integer(as.character(llid))), 
            by = c("lagoslakeid" = "llid")) %>%
  left_join(cdl, by = c("lagoslakeid" = "llid"))

saveRDS(dt, "data/dt.rds")
# dt <- readRDS("data/dt.rds")

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
  select(variable, units)

saveRDS(dt_units, "data/dt_units.rds")
