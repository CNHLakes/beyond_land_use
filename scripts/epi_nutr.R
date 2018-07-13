library(LAGOSNE)
library(dplyr)
library(magrittr)

lg <- lagosne_load("1.087.1")

# ---- get_tp ----

ep <- lg$epi_nutr %>% 
  select(lagoslakeid, sampledate, tp, tn) %>%
  filter(!is.na(tp) | !is.na(tn)) %>%
  group_by(lagoslakeid) %>%
  filter(sampledate > as.Date("1999-01-01") & sampledate < as.Date("2013-01-01")) %>%
  mutate(count = n(), min_date = min(sampledate), max_date = max(sampledate)) %>%
  filter(count >= 7) %>%
  summarize(tp = log(median(tp, na.rm = TRUE)), tn = log(median(tn, na.rm = TRUE))) %>%
  identity()

mi_ep <- ep %>%
  left_join(select(lg$locus, 
                   nhd_long, nhd_lat, state_zoneid, hu8_zoneid, lagoslakeid)) %>%
  left_join(select(lg$state, state_zoneid, state_name)) %>%
  left_join(select(lg$hu8, hu8_zoneid, hu8)) %>%
  mutate(hu8 = sprintf("%08d", hu8)) %>%
  mutate(hu6 = paste0(substring(hu8, 0, 6), "00")) %>%
  filter(state_name == "Michigan") %>%
  data.frame() %>%
  coordinatize()

# mapview::mapview(st_sf(test2), zcol  = "Value") + 
#   mapview::mapview(mi_ep, zcol = "tp")

# hist(ep$sampledate, "weeks")
# hist(ep$count, xlim = c(0, 100), n = 100)
# hist(ep$tn)

# ---- get_covariates ----

mi_ep <- left_join(mi_ep, select(lg$hu8.lulc, 
                        hu8_nlcd2006_ha_81, hu8_nlcd2006_ha_82, hu8_zoneid)) %>% 
  group_by(hu6) %>%
  mutate(hu6_ag_2006 = sum(hu8_nlcd2006_ha_81, hu8_nlcd2006_ha_82, na.rm = TRUE)) %>%
  data.frame() %>%
  identity()

saveRDS(mi_ep, "data/mi_ep.rds")