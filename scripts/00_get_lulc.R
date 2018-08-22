library(LAGOSNE)
library(dplyr)

lg <- lagosne_load("1.087.1")

# ---- get_iws_landcover ----

iws_lulc <- select(lg$iws.lulc, contains("nlcd2001_pct"), lagoslakeid) %>%
  group_by(lagoslakeid) %>%
  mutate(ag_pct = sum(iws_nlcd2001_pct_81,
                           iws_nlcd2001_pct_82, na.rm = TRUE),
         iws_ag_2001_pcent = ag_pct / 100) %>%
  select(row_crop_pct = iws_nlcd2001_pct_82, 
         pasture_pct = iws_nlcd2001_pct_81,
         ag_pct,
         iws_ag_2001_pcent,
         lagoslakeid) %>%
  data.frame()

saveRDS(iws_lulc, "data/iws_lulc.rds")

# ---- get_county_landcover ----

county_lulc <- dplyr::select(lg$county.lulc, contains("nlcd2001_ha"), county_zoneid) %>%
  group_by(county_zoneid) %>%
  mutate(county_ag_2001 = sum(county_nlcd2001_ha_81,
                              county_nlcd2001_ha_82, na.rm = TRUE)) %>%
  left_join(dplyr::select(lg$county, county_ha, county_zoneid)) %>%
  mutate(county_ag_2001_pcent = county_ag_2001 / county_ha) %>%
  data.frame()

saveRDS(county_lulc, "data/county_lulc.rds")