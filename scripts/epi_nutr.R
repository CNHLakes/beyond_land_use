library(LAGOSNE)
library(dplyr)
library(magrittr)
library(sf)
library(stringr)
library(mapview)

# prep lagos
lg         <- lagosne_load("1.087.1")
date_start <- as.Date("1995-01-01")
date_end   <- as.Date("2002-01-01")
ag_cutoff  <-
min_state_n <- 

ep <- lg$epi_nutr %>%
  select(lagoslakeid, sampledate, tp, tn) %>%
  filter(!is.na(tp) | !is.na(tn)) %>%
  group_by(lagoslakeid) %>%
  filter(sampledate > date_start & sampledate < date_end) %>%
  mutate(count = n(), min_date = min(sampledate), max_date = max(sampledate)) %>%
  filter(count > 3) %>%
  summarize(tp = log(median(tp, na.rm = TRUE)), tn = log(median(tn, na.rm = TRUE))) %>%
  identity()

saveRDS(ep, "data/ep.rds")

# pull hu8 polygons, spatial merge by hu6
gdb_path    <- path.expand("~/.local/share/LAGOS-GIS/lagos-ne_gis.gpkg")
hu8         <- st_read(gdb_path, "HU8")
hu8         <- filter(hu8, str_detect(States, "MI"))
hu8$HUC6    <- paste0(sapply(as.character(hu8$HUC8),
                             function(x) substring(x, 0, 6)), "00")
hu6 <- hu8 %>%
  st_simplify() %>%
  group_by(HUC6) %>%
  summarize() %>%
  mutate(hu6_area = st_area(geom))

# ---- get_hu6_landcover ----

hu6_lulc <- select(lg$hu8.lulc, contains("nlcd2006_ha"), hu8_zoneid) %>%
            left_join(select(lg$hu8, hu8, hu8_zoneid)) %>%
            mutate(hu8 = sprintf("%08d", hu8)) %>%
            mutate(hu6 = paste0(substring(hu8, 0, 6), "00")) %>%
            group_by(hu6) %>%
            mutate(hu6_ag_2006 = sum(hu8_nlcd2006_ha_81,
                                     hu8_nlcd2006_ha_82, na.rm = TRUE)) %>%
            ungroup() %>% data.frame() %>%
            mutate(hu8_area = select(., hu8_nlcd2006_ha_0:hu8_nlcd2006_ha_95) %>%
                   rowSums(na.rm = TRUE)) %>%
            group_by(hu6) %>%
            mutate(hu6_area = sum(hu8_area, na.rm = TRUE),
                   hu6_ag_2006_pcent = hu6_ag_2006 / hu6_area) %>%
            data.frame()

# ---- get_iws_landcover ----

iws_lulc <- select(lg$iws.lulc, contains("nlcd2006_ha"), lagoslakeid) %>%
  group_by(lagoslakeid) %>%
  mutate(iws_ag_2006 = sum(iws_nlcd2006_ha_81,
                           iws_nlcd2006_ha_82, na.rm = TRUE)) %>%
  left_join(select(lg$iws, iws_ha, lagoslakeid)) %>%
  mutate(iws_ag_2006_pcent = iws_ag_2006 / iws_ha) %>%
  data.frame()

saveRDS(iws_lulc, "data/iws_lulc.rds")

# ---- get_county_landcover ----

county_lulc <- dplyr::select(lg$county.lulc, contains("nlcd2006_ha"), county_zoneid) %>%
  group_by(county_zoneid) %>%
  mutate(county_ag_2006 = sum(county_nlcd2006_ha_81,
                              county_nlcd2006_ha_82, na.rm = TRUE)) %>%
  left_join(dplyr::select(lg$county, county_ha, county_zoneid)) %>%
  mutate(county_ag_2006_pcent = county_ag_2006 / county_ha) %>%
  data.frame()

saveRDS(county_lulc, "data/county_lulc.rds")

# ---- get_tp ----

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

# ---- get_covariates ----

mi_ep <- left_join(mi_ep, select(iws_lulc, lagoslakeid, iws_ag_2006_pcent))
mi_ep <- left_join(mi_ep, data.frame(select(hu6, HUC6, hu6_area)), by = c("hu6" = "HUC6"))

# mapview(mi_ep, zcol = "hu6_ag_2006_pcent")

saveRDS(mi_ep, "data/mi_ep.rds")
