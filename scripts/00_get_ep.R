library(LAGOSNE)
library(dplyr)

lg           <- lagosne_load("1.087.1")
iws_lulc     <- readRDS("data/iws_lulc.rds")
county_lulc  <- readRDS("data/county_lulc.rds")
date_start   <- as.Date("1995-01-01")
date_end     <- as.Date("2002-01-01")
min_sample_n <- 3
ag_cutoff    <- 0.4
min_state_n  <- 4

# filter ep with tn/tp data meeting date and n constraints  
ep_nutr <- lg$epi_nutr %>%
  select(lagoslakeid, sampledate, tp, tn) %>%
  filter(!is.na(tp) | !is.na(tn)) %>%
  group_by(lagoslakeid) %>%
  filter(sampledate > date_start & sampledate < date_end) %>%
  mutate(count = n(), min_date = min(sampledate), max_date = max(sampledate)) %>%
  filter(count > min_sample_n) %>%
  summarize(tp = log(median(tp, na.rm = TRUE)), tn = log(median(tn, na.rm = TRUE))) %>%
  identity()

# filter ep with ag above cutoff
iws_vs_county_ag <- dplyr::select(iws_lulc, lagoslakeid,
                                  iws_ag_2006:iws_ag_2006_pcent) %>%
  left_join(dplyr::select(lg$locus, lagoslakeid, county_zoneid))  %>%
  left_join(dplyr::select(county_lulc, county_zoneid:county_ag_2006_pcent))

get_ag_cutoff <- function(cutoff){
  hi_ag_counties <- dplyr::filter(iws_vs_county_ag,
                                  county_ag_2006_pcent >= cutoff &
                                    iws_ag_2006_pcent >= cutoff) %>%
    group_by(county_zoneid) %>%
    summarize(n_lakes = n()) %>%
    dplyr::filter(n_lakes >= 1) %>%
    left_join(dplyr::select(lg$county, county_zoneid, county_state, county_name))
  
  hi_ag_iws <- dplyr::filter(iws_vs_county_ag, 
                             iws_ag_2006_pcent >= cutoff) %>%
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

saveRDS(ep, "data/ep.rds")