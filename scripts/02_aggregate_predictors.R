library(dplyr)
library(magrittr)

# response variables are medians from 1995 - 2005
ep <- readRDS("data/ep.rds")

# predictors are at the iws level and are as close in time to 2000 as possible
lg_lulc <- readRDS("data/iws_lulc.rds") %>%
  select(-iws_ag_2001_pcent)

usgs <- readRDS("data/usgs/usgs.rds") %>%
  select(lagoslakeid, phosphorus_input, nitrogen_input, n_dep)

dt <- ep %>%
  left_join(lg_lulc) %>%
  left_join(usgs)

saveRDS(dt, "data/dt.rds")
