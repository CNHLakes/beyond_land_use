# ---- lagos_setup ----
# setwd("_episodes_rmd")
library(vapour)
library(LAGOSNE)
library(magrittr)
library(dplyr)
library(lme4)
library(sf)
library(units)
library(merTools)
library(ggplot2)
library(cowplot)
library(ggridges)
library(tidyr)
library(ggExtra)

gdb_path <- path.expand("~/.local/share/LAGOS-GIS/lagos-ne_gis.gpkg")
# st_layers(gdb_path)
layer_name <- "COUNTY"
county <- st_read(gdb_path, layer_name, quiet = TRUE)
states <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))

pull_ag_polygons <- function(hi_ag_iws, county){
  layer_name <- "IWS"
  crs <- st_crs(county)
  # system(paste("ogrinfo -so", gdb_path, layer_name))
  query      <- paste0("SELECT * FROM ", layer_name,
                       " WHERE lagoslakeid IN ('",
                       paste0(hi_ag_iws$lagoslakeid,
                              collapse = "', '"), "');")
  iws <- st_as_sfc(vapour_read_geometry(gdb_path, sql = query))
  iws <- st_sf(vapour_read_attributes(gdb_path, sql = query),
               geometry = iws, crs = crs)
  iws <- st_zm(iws)
  iws <- st_cast(iws, "MULTIPOLYGON") # solves error: Unknown WKB type 12

  ## filter counties that intersect hi ag iws
  hi_ag_counties_intersects <- st_intersects(county, iws)
  hi_ag_counties_intersects <- unlist(lapply(hi_ag_counties_intersects,
                                             function(x) length(x > 0)))
  county <- county[hi_ag_counties_intersects > 0,]

  ## get number of counties intersecting hi ag iws
  hi_ag_iws_intersects <- st_intersects(iws, county)
  hi_ag_iws_intersects <- unlist(lapply(hi_ag_iws_intersects,
                                        function(x) length(x > 0)))

  list(iws = iws,
       county = county,
       counties_intersects = hi_ag_counties_intersects,
       iws_intersects = hi_ag_iws_intersects)
}

get_iws_polygon <- function(lagoslakeid, crs){
  layer_name <- "IWS"
  # system(paste("ogrinfo -so", gdb_path, layer_name))
  query      <- paste0("SELECT * FROM ", layer_name,
                       " WHERE lagoslakeid IN ('",
                       paste0(lagoslakeid,
                              collapse = "', '"), "');")
  iws <- st_as_sfc(vapour_read_geometry(gdb_path, sql = query))
  iws <- st_sf(vapour_read_attributes(gdb_path, sql = query),
               geometry = iws, crs = crs)
  iws <- st_zm(iws)
  st_cast(iws, "MULTIPOLYGON") # solves error: Unknown WKB type 12
}

lg          <- lagosne_load("1.087.1")
iws_lulc    <- readRDS("lagos_ag/data/iws_lulc.rds")
county_lulc <- readRDS("lagos_ag/data/county_lulc.rds")

iws_vs_county_ag <- dplyr::select(iws_lulc, lagoslakeid,
                                  iws_ag_2006:iws_ag_2006_pcent) %>%
  left_join(dplyr::select(lg$locus, lagoslakeid, county_zoneid))  %>%
  left_join(dplyr::select(county_lulc, county_zoneid:county_ag_2006_pcent))

# ---- iws_vs_county_scatter ----
# iws vs county ag scatter plot
# setwd("_episodes_rmd")
gg <- ggplot(data = iws_vs_county_ag) +
  geom_point(aes(x = county_ag_2006_pcent, y = iws_ag_2006_pcent), alpha = 0.5) +
  xlab("NLCD 2006 County Ag (%)") + ylab("NLCD 2006 IWS Ag (%)") + ylim(0, 1) +
  ggtitle("Distribution of county and iws ag")

ggMarginal(gg, fill = "gray")

# ---- hi_ag_county_map ----

# Geography of high ag counties
cutoff <- 0.75
hi_ag_counties <- get_ag_cutoff(cutoff = cutoff)$hi_ag_counties
hi_ag_iws      <- get_ag_cutoff(cutoff = cutoff)$hi_ag_iws

knitr::kable(
  t(setNames(data.frame(
    table(hi_ag_counties$county_state)),
    c("state", paste0("# of high ( >", cutoff, ") ag counties")))))

pg <- plot_grid(
  ggplot() +
    geom_sf(data = county) +
    geom_sf(data = dplyr::filter(county,
                                 ZoneID %in% get_ag_cutoff(cutoff = 0.25)$
                                   hi_ag_counties$county_zoneid),
            color = "red") +
    ggtitle("0.25 ag"),
  ggplot() +
    geom_sf(data = county) +
    geom_sf(data = dplyr::filter(county,
                                 ZoneID %in% get_ag_cutoff(cutoff = 0.50)$
                                   hi_ag_counties$county_zoneid),
            color = "red") +
    ggtitle("0.50 ag"),
  ggplot() +
    geom_sf(data = county) +
    geom_sf(data = dplyr::filter(county,
                                 ZoneID %in% get_ag_cutoff(cutoff = 0.75)$
                                   hi_ag_counties$county_zoneid),
            color = "red") +
    ggtitle("0.75 ag"),
  ncol = 1)

pg
# title <- ggdraw() + draw_label("Hi ag counties", fontface='bold')
# plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

# ---- hi_ag_iws_map ----

## pull hi ag iws polygons
# average number of counties covered by an iws
ag_iws      <- get_ag_cutoff(cutoff = 0.70)$hi_ag_iws
ag_polygons <- pull_ag_polygons(ag_iws, county)

# geography of high ag counties
plot_grid(
  ggplot() +
    geom_sf(data = county) +
    geom_sf(data = pull_ag_polygons(
      get_ag_cutoff(cutoff = 0.70)$hi_ag_iws, county)$county, color = "red") +
    # theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    ggtitle("70% iws ag") +
    NULL,
  ggplot() +
    geom_sf(data = county) +
    geom_sf(data = pull_ag_polygons(
      get_ag_cutoff(cutoff = 0.80)$hi_ag_iws, county)$county, color = "red") +
    # theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    ggtitle("80% iws ag") +
    NULL,
  ggplot() +
    geom_sf(data = county) +
    geom_sf(data = pull_ag_polygons(
      get_ag_cutoff(cutoff = 0.90)$hi_ag_iws, county)$county, color = "red") +
    # theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    ggtitle("90% iws ag") +
    NULL,
  ncol = 1)

# ---- hi_ag_iws_w_ep ----
# counties that overlap hi ag iws with ep data
# setwd("_episodes_rmd")
ep <- readRDS("lagos_ag/data/ep.rds")

hi_ag_iws_w_ep <- pull_ag_polygons(ep, county)

n_iws <- unlist(lapply(
  st_intersects(hi_ag_iws_w_ep$county, hi_ag_iws_w_ep$iws),
  function(x) length(x)))

hi_ag_iws_w_ep$county$n_iws <- n_iws

states <- st_transform(states, st_crs(hi_ag_iws_w_ep$county))
state_intersects <- st_intersects(states, hi_ag_iws_w_ep$county)
state_intersects <- states[unlist(lapply(
  state_intersects,
  function(x) length(x) > 3)),]

hi_ag_iws_w_ep$county <- hi_ag_iws_w_ep$county[
  unlist(lapply(st_intersects(hi_ag_iws_w_ep$county, state_intersects), function(x) length(x) > 0)),]

# color by number of iws intersections
ggplot() +
  geom_sf(data = state_intersects) +
  geom_sf(data = hi_ag_iws_w_ep$county, aes(fill = n_iws)) +
  ggtitle(paste0("# of overlappling IWS with > 40% ag & epi nutrient data \n
          where > 4 measurements between 1995 and 2005")) +
  theme(title = element_text(size = 10))

# mapview::mapview(hi_ag_iws_w_ep$county, zcol = "n_iws")

cat(paste0(nrow(ep), " total lakes"))
