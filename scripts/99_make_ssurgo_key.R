# Deriving MUID average values for permeability, available water
# capacity, bulk density, organic matter content, soil erodibility
# factor, soil loss tolerance factor, and wind erodibility group
# required that a weighted average value be computed for all the soil
# layers in a soil component, and then a weighted average value be
# computed for all the soil components in a soil mapping unit. 
# 
# Each step of the two-step weighting process was done exactly as described 
# above, except first the layer values were weighted by their thicknesses, 
# and then the component values were weighted by their percentage composition.

# setup ####
library(dplyr)

res <- data.frame("metric" = NULL, "query" = NULL)

gpkg_path <- path.expand("~/Documents/Science/Data/gssurgo_data/gpkgs/")
in_gpkg <- list.files(gpkg_path, pattern = ".gpkg", 
                      full.names = TRUE, include.dirs = TRUE)[1]
con <- src_sqlite(in_gpkg)
# src_tbls(con)

# wetland_potential ####
qry <- tbl(con, "Valu1") %>%
  dplyr::select(mukey, pwsl1pomu) %>%
  dbplyr::sql_render() %>%
  stringr::str_replace_all(c("`" = "", "\n" = " "))

res <- bind_rows(res,
                 c(metric = "wetland_potential", 
                   query = qry,
                   agg_type = "percent"))

# soil_org_carbon ####
qry <- tbl(con, "Valu1") %>%
  dplyr::select(mukey, soc0_20) %>%
  dbplyr::sql_render() %>%
  stringr::str_replace_all(c("`" = "", "\n" = " "))

res <- bind_rows(res,
                 c(metric = "soil_org_carbon", 
                   query = qry, 
                   agg_type = "mean"))

# clay_pct ####
qry <- dplyr::select(dplyr::tbl(con, "chorizon"), 
                     claytotal_r, dbthirdbar_r, hzname, hzthk_r, cokey) %>%
  # First, the component average values were computed by weighting the layer
  # values by their thickness. 
  mutate(hzthk_r = coalesce(hzthk_r, 1)) %>%
  left_join(x = ., 
            y = summarise(group_by(., cokey), 
                          thk_sum = sum(hzthk_r, na.rm = TRUE) + 0.0), 
            by = "cokey") %>%
  mutate(thk_frac = hzthk_r / thk_sum, 
         clay_percent = claytotal_r / dbthirdbar_r,
         clay_percent_weighted = clay_percent * thk_frac) %>%
  inner_join(x = ., 
             y = summarize(group_by(., cokey),
                           clay_percent_co = sum(clay_percent_weighted, na.rm = TRUE)), 
             by = "cokey") %>%
  distinct(cokey, clay_percent_co) %>%
  # Second, the MUID average was computed by weighting the component average 
  # values by their percentage composition of the MUID. 
  left_join(dplyr::select(tbl(con, "component"), 
                      comppct_r, cokey, mukey), .) %>%
  mutate(clay_percent_mu_sum = (comppct_r / 100) * clay_percent_co) %>%
  inner_join(x = ., 
             y = summarize(group_by(., mukey),
                           clay_percent_mu = sum(clay_percent_mu_sum, na.rm = TRUE)), 
             by = "mukey") %>%
  distinct(mukey, clay_percent_mu) %>%
  # collect()
  dbplyr::sql_render() %>%
  stringr::str_replace_all(c("`" = "", "\n" = " "))

res <- bind_rows(res,
                 c(metric = "clay_pct", 
                   query = qry, 
                   agg_type = "percent"))

# sql_query = "SELECT mukey, AVG(kwfact) AS kwfact FROM (SELECT * FROM (SELECT TBL_LEFT.mukey AS mukey, TBL_LEFT.cokey AS cokey, TBL_LEFT.majcompflag AS majcompflag, TBL_RIGHT.hzname AS hzname, TBL_RIGHT.kwfact AS kwfact   FROM (SELECT mukey, cokey, majcompflag FROM component) AS TBL_LEFT   LEFT JOIN (SELECT hzname, kwfact, cokey FROM chorizon) AS TBL_RIGHT   ON (TBL_LEFT.cokey = TBL_RIGHT.cokey) ) WHERE ((hzname = 'A') AND (majcompflag = 'Yes'))) GROUP BY mukey"

# save key ####
write.csv(res, "data/gssurgo/gssurgo_key.csv", row.names = FALSE)

# validate queries ####
con <- DBI::dbConnect(RSQLite::SQLite(), in_gpkg)

# get idea of values (check numeric)
test <- data.frame(t(sapply(res[,"query"], function(x) 
  DBI::dbGetQuery(con, paste(x, "LIMIT 1")))))
row.names(test) <- NULL
setNames(test, c("mukey", "value"))

# test specific queries
# test <- DBI::dbGetQuery(con, qry)
# hist(test$clay_percent_t)
# arrange(test, desc(clay_percent_t)) %>%
#    head(n = 20)
