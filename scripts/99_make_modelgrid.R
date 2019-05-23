
pred_key <- read.csv("data/predictor_key.csv", stringsAsFactors = FALSE)
pred_key <- dplyr::filter(pred_key, !(var %in% c("lake_cultivated_crops", "wetlands", 
                                                 "wheat", "nonag", "nitrogen_livestock_manure", 
                                                 "lake_area_ha", "iwsla_ratio", 
                                                 "hu12_ppt_mean",
                                                 "nitrogen_atmospheric_deposition", 
                                                 "phosphorus_livestock_manure", 
                                                 "p_input")))

# pick one of baseflow or precip maybe the finest resolution?
# pick just maxdepth
# pick _one_ of the "best" t_capacity for n/p
# pick _one_ of the "best" nut_source for n/p

n_l <- list()
p_l <- list()

(l <- list(lake = unique(dplyr::filter(pred_key, category == "Lake")$var), 
          t_capacity = unique(dplyr::filter(pred_key, category == "Transport capacity")$var),
          nut_prox = unique(dplyr::filter(pred_key, category == "Nutrient proxies")$var),
          nut_source = unique(dplyr::filter(pred_key, category == "Nutrient sources")$var)))

expand.grid(l)


