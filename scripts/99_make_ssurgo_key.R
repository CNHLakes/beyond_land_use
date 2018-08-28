library(dplyr)

res <- data.frame("metric" = NULL, "query" = NULL)

gpkg_path <- path.expand("~/Documents/Science/Data/gssurgo_data/gpkgs/")
in_gpkg <- list.files(gpkg_path, pattern = ".gpkg", 
                      full.names = TRUE, include.dirs = TRUE)[1]
con <- src_sqlite(in_gpkg)
# src_tbls(con)

# wetland_potential ####
qry <- tbl(con, "Valu1") %>%
  select(mukey, pwsl1pomu) %>%
  dbplyr::sql_render() %>%
  stringr::str_replace_all(c("`" = "", "\n" = " "))

res <- bind_rows(res,
                 c(metric = "wetland_potential", query = qry))

# soil_org_carbon ####
qry <- tbl(con, "Valu1") %>%
  select(mukey, soc0_999) %>%
  dbplyr::sql_render() %>%
  stringr::str_replace_all(c("`" = "", "\n" = " "))

res <- bind_rows(res,
                 c(metric = "soil_org_carbon", query = qry))

write.csv(res, "data/gssurgo/gssurgo_key.csv", row.names = FALSE)

# validate queries ####
con <- DBI::dbConnect(RSQLite::SQLite(), in_gpkg)
test <- data.frame(t(sapply(res[,"query"], function(x) 
  DBI::dbGetQuery(con, paste(x, "LIMIT 1")))))
setNames(test, c("mukey", "value"))
