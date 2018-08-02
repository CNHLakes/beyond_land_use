# ---- cdl_setup ----
library(cdlTools)
library(raster)
library(mapview)
library(reticulate)
library(dplyr)
library(tabularaster)
library(sf)
library(ggplot2)
library(LAGOSNE)
library(gdalUtils)
library(LAGOSextra)

# ---- cdl_get_mi ----

cdl_mi <- cdlTools::getCDL("MI", 2012, ssl.verifypeer = FALSE)
writeRaster(cdl_mi$MI2012, "_episodes_rmd/lagos_ag/data/cdl_mi.tif")

system(paste("gdalwarp -tr 120 120 -r average",
             "_episodes_rmd/lagos_ag/data/cdl_mi.tif",
             "_episodes_rmd/lagos_ag/data/cdl_mi_aggregate.tif"))

# ---- cdl_waffle ----
# setwd("_episodes_rmd")
cdl_mi <- raster("lagos_ag/data/cdl_mi_aggregate.tif")

cdl_table <- as_data_frame(table(values(cdl_mi)), stringsAsFactors = FALSE) %>%
              mutate(Var1 = as.integer(Var1))

cdl_key <- data.frame(code = 0:255, description = cdlTools::updateNamesCDL(0:255),
                      stringsAsFactors = FALSE)
cdl_key <- cdl_key[which(nchar(cdl_key$description) > 3),]
aggregate_categories <- function(cdl_key){
  res <- mutate(cdl_key, category = case_when(
    grepl("wheat", tolower(description)) ~ "wheat",
    grepl("wetlands", tolower(description)) ~ "wetlands",
    grepl("developed", tolower(description)) ~ "developed",
    grepl("dbl", tolower(description)) ~ "mixed crop",
    grepl("background", tolower(description)) ~ "background",
    grepl("barren", tolower(description)) ~ "other non ag",
    grepl("corn", tolower(description)) ~ "corn",
    grepl("soybeans", tolower(description)) ~ "soybeans",
    grepl("alfalfa", tolower(description)) ~ "alfalfa",
    grepl("pasture", tolower(description)) ~ "pasture",
    grepl("grassland", tolower(description)) ~ "other non ag",
    grepl("shrubland", tolower(description)) ~ "other non ag",
    grepl("forest", tolower(description)) ~ "forest",
    grepl("nonag", tolower(description)) ~ "other non ag",
    grepl("water", tolower(description)) ~ "water",
    grepl("aqua", tolower(description)) ~ "other non ag",
    grepl("ice", tolower(description)) ~ "other non ag",
    grepl("clouds", tolower(description)) ~ "other non ag"
  )) %>%
    tidyr::replace_na(list(category = "other ag"))

  res <- mutate(res, is_ag = if_else(category %in% c("corn", "wheat", "other ag",
                                             "soybeans", "mixed crop", "pasture",
                                             "alfalfa"),
                                     "ag", "nonag"))
  res
}

cdl_key <- aggregate_categories(cdl_key)

cdl_freq <- left_join(cdl_key, cdl_table, by = c("code" = "Var1")) %>%
  filter(description != "Background") %>%
  arrange(desc(n)) %>%
  group_by(category) %>%
  summarize(group_n = sum(n, na.rm = TRUE), is_ag = first(is_ag)) %>%
  ungroup() %>%
  mutate(percent = round((group_n / sum(group_n, na.rm = TRUE)) * 100, 0)) %>%
  filter(percent >= 1) %>%
  arrange(is_ag, percent)
cdl_freq <- add_row(cdl_freq, category = "other", group_n = NA,
                    percent = 100 - sum(cdl_freq$percent, na.rm = TRUE))

category_order <- arrange(cdl_freq, is_ag, percent)$category

cdl_isag <- left_join(cdl_key, cdl_table, by = c("code" = "Var1")) %>%
  filter(description != "Background") %>%
  arrange(desc(n)) %>%
  group_by(is_ag) %>%
  summarize(group_n = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent = round((group_n / sum(group_n, na.rm = TRUE)) * 100, 0)) %>%
  arrange(percent)

# plotting
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)

cdl_freq_vector <- rep(cdl_freq$category, times = cdl_freq$percent)
categ_table <- round(table(cdl_freq_vector) *
                       ((nrows*nrows)/(length(cdl_freq_vector))))
categ_table <- categ_table[order(categ_table, decreasing = TRUE)]
categ_table <- categ_table[c(2:length(categ_table), 1)]
categ_table <- factor(rep(names(categ_table), categ_table), levels = category_order)
df$category <- categ_table[order(categ_table)]

cdl_isna_vector <- rep(cdl_isag$is_ag, times = cdl_isag$percent)
categ_table <- round(table(cdl_isna_vector) *
                       ((nrows*nrows)/(length(cdl_isna_vector))))
categ_table <- categ_table[order(categ_table, decreasing = TRUE)]
categ_table <- categ_table[c(2:length(categ_table), 1)]
df$isag <- factor(rep(names(categ_table), categ_table))

gg_theme <- theme(plot.title = element_text(size = rel(1.2)),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  legend.title = element_blank(),
                  legend.position = "right")

ggplot(df, aes(x = x, y = y, fill = category)) +
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  gg_theme

ggplot(df, aes(x = x, y = y, fill = isag)) +
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  gg_theme

# ---- cdl_county ----

# select some contigous iws

# lg <- lagosne_load("1.087.1")
# wislakes <- query_wbd(lagoslakeid = c(5371, 4559), utm = FALSE)
# st_bbox(wislakes)
bbox <- c(517599.5, 2244764.7, 545008.9, 2270068.9)
gdb_path <- path.expand("~/.local/share/LAGOS-GIS/lagos-ne_gis.gpkg")
tfile    <- tempfile(fileext = ".gpkg")
invisible(ogr2ogr(gdb_path, tfile, layer = "IWS", spat = bbox, f = "GPKG"))
iws      <- st_read(tfile, quiet = TRUE) %>%
  mutate(ZoneID = gsub("IWS_", "", ZoneID))

# select overlapping counties
county_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
county_sf <- tidyr::separate(county_sf, ID, c("state", "county"))
county_sf <- st_transform(county_sf, st_crs(iws))
county_sf <- county_sf[
  unlist(lapply(
    st_intersects(county_sf, iws),
    function(x) length(x) > 0)),]

# mapview(county_sf) + mapview(iws)

# join lagos data to counties
lg        <- lagosne_load("1.087.1")
lg_county <- mutate(lg$county,
                    county_name = tolower(gsub(" County", "", county_name))) %>%
  select(county_state, county_name, county_zoneid) %>%
  left_join(select(lg$county.lulc, contains("nlcd2011_ha_81"), county_zoneid)) %>%
  left_join(data.frame(state = state.name, county_state = state.abb,
                       stringsAsFactors = FALSE)) %>%
  mutate(state = tolower(state), county = tolower(county_name))
county_sf <- left_join(county_sf, lg_county, by = c("state", "county"))

# interpolate county data to iws
  # use sf::st_interpolate_aw()

iws_interp <- st_interpolate_aw(county_sf["county_nlcd2011_ha_81"], iws,
                          extensive = TRUE)
iws_interp <- data.frame(iws_nlcd2011_ha_81_interp = iws_interp$county_nlcd2011_ha_81,
                         ZoneID = gsub("IWS_", "", as.character(iws$ZoneID)),
                         stringsAsFactors = FALSE)

# check against iws values
iws_base <- mutate(lg$iws, ZoneID = gsub("IWS_", "", iws_zoneid)) %>%
  dplyr::filter(ZoneID %in% iws$ZoneID) %>%
  left_join(mutate(dplyr::select(lg$iws.lulc, iws_zoneid, iws_nlcd2011_ha_81), ZoneID = gsub("IWS_", "", iws_zoneid))) %>%
  dplyr::select(contains("nlcd"), ZoneID) %>%
  mutate(ZoneID = gsub("IWS_", "", ZoneID)) %>%
  left_join(iws_interp) %>%
  mutate(error = iws_nlcd2011_ha_81 - iws_nlcd2011_ha_81_interp,
         ape = Metrics::ape(iws_nlcd2011_ha_81,
                            iws_nlcd2011_ha_81_interp),
         original = iws_nlcd2011_ha_81,
         interp = iws_nlcd2011_ha_81_interp)# ,
         # ape = replace(ape, ape == Inf, 20))

res <- left_join(iws, iws_base)

# ggplot(data = res) +
#   geom_histogram(aes(x = error))
# ggplot(data = res) +
#   geom_point(aes(x = IWSAreaHa, y = ape))

ggplot(data = res) +
  geom_point(aes(x = original, y = interp)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("LAGOS/NLCD Ag ha")

ggplot() +
  geom_sf(data = res, aes(fill = ape)) +
  geom_sf(data = county_sf, alpha = 0.1, size = 1) +
  coord_sf(datum = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_continuous(name = "abs % error")

ggplot() +
  geom_sf(data = res, aes(fill = error)) +
  geom_sf(data = county_sf, alpha = 0.1, size = 1) +
  coord_sf(datum = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_distiller(name = "error", type = "div")

# mapview(res, zcol = "error")
# mapview(res, zcol = "ape")
