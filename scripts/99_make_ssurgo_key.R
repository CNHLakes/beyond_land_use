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
qry <- dplyr::tbl(con, "chorizon") %>%
  dplyr::select(claytotal_r, dbthirdbar_r, hzname, hzdepb_r, cokey) %>%
  left_join(dplyr::select(tbl(con, "component"), comppct_r, cokey, mukey)) %>%
  dplyr::mutate(clay_percent = claytotal_r / dbthirdbar_r) %>% 
  # pick shallowest horizon
  inner_join(x = ., y = summarize(group_by(., mukey), min_hzdep = min(hzdepb_r, na.rm = TRUE)), 
             by = "mukey") %>%
  dplyr::filter(hzdepb_r == min_hzdep) %>%
  ####
  # compute component clay average
  inner_join(x = ., 
             y = summarize(group_by(., mukey), 
                           min_cokey = min(cokey, na.rm = TRUE),
                           clay_percent_t = sum(clay_percent * (comppct_r / 100), na.rm = TRUE)), by = "mukey") %>%
  dplyr::filter(cokey == min_cokey) %>%
  select(mukey, clay_percent_t) %>%
  ####
  dbplyr::sql_render() %>%
  stringr::str_replace_all(c("`" = "", "\n" = " "))

res <- bind_rows(res,
                 c(metric = "clay_pct", 
                   query = qry, 
                   agg_type = "percent"))

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
