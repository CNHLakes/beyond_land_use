
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

n_l <- list(lake = "maxdepth", 
            t_capacity = "soil_org_carbon", 
            nut_source = "nitrogen_fertilizer_use", 
            nut_prox = unique(dplyr::filter(pred_key, category == "Nutrient proxies")$var))
p_l <- list(lake = "maxdepth", 
            t_capacity = "hu12_baseflow_mean", 
            nut_source = "nitrogen_fertilizer_use",  
            nut_prox = unique(dplyr::filter(pred_key, category == "Nutrient proxies")$var))

expand.grid(p_l)
expand.grid(n_l)
