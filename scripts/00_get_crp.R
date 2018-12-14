source("scripts/99_utils.R")
library(macroag)
library(dplyr)

names(crp) <- tolower(names(crp))
crp$state <- tolower(crp$state)
crp$county <- tolower(crp$county)
crp <- left_join(crp, county_sf(), 
                          by = c("state", "county"))
st_geometry(crp) <- crp$geometry

saveRDS(crp, "data/macroag/crp.rds")