library(LAGOSNE)
library(dplyr)

lg <- lagosne_load("1.087.1")

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