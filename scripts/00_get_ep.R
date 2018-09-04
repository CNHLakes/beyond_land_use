library(LAGOSNE)
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))

source("scripts/utils.R")

lg           <- lagosne_load("1.087.1")
iws_lulc     <- readRDS("data/iws_lulc.rds")
county_lulc  <- readRDS("data/county_lulc.rds")
date_start   <- as.Date("1995-01-01")
date_end     <- as.Date("2005-01-01")
min_sample_n <- 3
ag_cutoff    <- 0.4
min_state_n  <- 4
max_iws_ha   <- 190000
max_lake_area_ha <- 40000

fix_wi_lkls <- function(lg){
  lg$tkn[which(lg$programname=="WI_LKLS")] <-
    lg$tkn[which(lg$programname=="WI_LKLS")] * 1000

  lg$no2no3[which(lg$programname=="WI_LKLS")] <-
    lg$no2no3[which(lg$programname=="WI_LKLS")] * 1000
  
  lg
}

calculate_tn <- function(lg){
  lg$tn_calculated <- lg$tkn + lg$no2no3
  lg$tn_combined   <- lg$tn
  lg$tn_combined[which(is.na(lg$tn_combined) == TRUE)] <-
    lg$tn_calculated[which(is.na(lg$tn_combined) == TRUE)]
  lg$tn <- lg$tn_combined
  lg
}

# filter ep with tn/tp data meeting date and n constraints
ep_nutr <- lg$epi_nutr %>%
  fix_wi_lkls() %>%
  calculate_tn() %>%
  select(lagoslakeid, sampledate, tp, tn, no2no3) %>%
  filter(!is.na(tp) | !is.na(tn) | !is.na(no2no3)) %>%
  group_by(lagoslakeid) %>%
  filter(sampledate > date_start & sampledate < date_end) %>%
  mutate(count = n(), min_date = min(sampledate), max_date = max(sampledate)) %>%
  filter(count > min_sample_n) %>%
  summarize(tp = median(tp, na.rm = TRUE), 
            tn = median(tn, na.rm = TRUE),
            no2no3 = median(no2no3, na.rm = TRUE)) %>%
  identity()

# filter ep with ag above cutoff
iws_vs_county_ag <- iws_lulc %>%
  left_join(dplyr::select(lg$locus, lagoslakeid, county_zoneid), 
            by = "lagoslakeid")  %>%
  left_join(dplyr::select(county_lulc, county_zoneid:county_ag_2001_pcent), 
            by = "county_zoneid")

get_ag_cutoff <- function(cutoff){
  hi_ag_counties <- dplyr::filter(iws_vs_county_ag,
                                  county_ag_2001_pcent >= cutoff &
                                  iws_ag_2001_pcent >= cutoff) %>%
    group_by(county_zoneid) %>%
    summarize(n_lakes = n()) %>%
    dplyr::filter(n_lakes >= 1) %>%
    left_join(dplyr::select(lg$county, county_zoneid, county_state, county_name))

  hi_ag_iws <- dplyr::filter(iws_vs_county_ag,
                             iws_ag_2001_pcent >= cutoff) %>%
    distinct(lagoslakeid)

  list(hi_ag_counties = hi_ag_counties, hi_ag_iws = hi_ag_iws)
}

ep <- get_ag_cutoff(cutoff = ag_cutoff)$hi_ag_iws %>%
  filter(lagoslakeid %in% ep_nutr$lagoslakeid)

# filter ep concentrated by state
ep <- left_join(ep, dplyr::select(lg$locus,
                                  lagoslakeid, state_zoneid,
                                  nhd_long, nhd_lat))
ep <- ep %>%
  group_by(state_zoneid) %>%
  summarize(n_count = n()) %>%
  right_join(ep) %>%
  filter(n_count >= min_state_n) %>%
  select(lagoslakeid:nhd_lat) %>%
  left_join(ep_nutr)

# add focal predictors
ep <- ep %>%
  left_join(dplyr::select(lg$iws, lagoslakeid, iws_ha)) %>%
  left_join(dplyr::select(lg$locus, lagoslakeid, lake_area_ha)) %>%
  mutate(iwsla_ratio = iws_ha / lake_area_ha)

# filter focal predictors
ep <- dplyr::filter(ep, iws_ha <= max_iws_ha & 
                      lake_area_ha <= max_lake_area_ha)

# filter geographic extent
states <- dplyr::filter(state_sf(), ABB != "ME")
ep <- ep[unlist(
  lapply(st_intersects(coordinatize(ep), states), function(x) length(x) != 0)),]

saveRDS(ep, "data/ep.rds")
# test <- readRDS("data/ep.rds")

# mapview::mapview(coordinatize(ep))
