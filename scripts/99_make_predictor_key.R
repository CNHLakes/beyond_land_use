pred_key <- data.frame(var = c(
  "nitrogen_atmospheric_deposition","maxdepth","iwsla_ratio",
  "hu12_baseflow_mean","hu12_ppt_mean","lake_cultivated_crops", 
  "stream_cultivated_crops","wetlands","ag", 
  "nonag","clay_pct","lake_area_ha", 
  "wetland_potential", "corn","nitrogen_livestock_manure", 
  "soil_org_carbon", "soybeans","n_input", 
  "nitrogen_fertilizer_use", "phosphorus_fertilizer_use","p_input", 
  "phosphorus_livestock_manure", "wheat","pasture"),
                      category = c(
  "nutrient_sources", "lake", "lake", 
  "transport_capacity", "transport_capacity", "nutrient_proxies",
  "nutrient_proxies","nutrient_proxies","nutrient_proxies",
  "nutrient_proxies", "transport_capacity","lake",
  "transport_capacity", "nutrient_proxies","nutrient_sources", 
  "nutrient_proxies", "nutrient_proxies", "nutrient_sources",
  "nutrient_sources", "nutrient_sources", "nutrient_sources", 
  "nutrient_sources", "nutrient_proxies","nutrient_proxies"),
                      stringsAsFactors = FALSE)

# View(pred_key)
write.csv(pred_key, "data/predictor_key.csv", row.names = FALSE)