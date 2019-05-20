pred_key <- data.frame(var = c(
  "nitrogen_atmospheric_deposition","maxdepth","iwsla_ratio",
  "hu12_baseflow_mean","hu12_ppt_mean","lake_cultivated_crops", 
  "stream_cultivated_crops","wetlands","ag", 
  "nonag","clay_pct","lake_area_ha", 
  "wetland_potential", "corn","nitrogen_livestock_manure", 
  "soil_org_carbon", "soybeans","n_input", 
  "nitrogen_fertilizer_use", "phosphorus_fertilizer_use","p_input", 
  "phosphorus_livestock_manure", "wheat","pasture"),
                      pretty = c(
    "N deposition", "Max depth", "Watershed-lake ratio", 
    "Baseflow", "Precipitation", "Lake-buffer Ag", 
    "Stream-buffer Ag", "Wetlands", "Ag",
    "Non-ag", "Clay", "Lake area",
    "Wetland potential", "Corn", "Manure N input", 
    "Soil organic carbon", "Soybeans", "N input",
    "Fertilizer N input", "Fertilizer P input", "P input", 
    "Manure P input", "Wheat", "Pasture"),
                      category = c(
  "Nutrient sources", "Lake", "Lake", 
  "Transport capacity", "Transport capacity", "Nutrient proxies",
  "Nutrient proxies","Nutrient proxies","Nutrient proxies",
  "Nutrient proxies", "Transport capacity","Lake",
  "Transport capacity", "Nutrient proxies","Nutrient sources", 
  "Nutrient proxies", "Nutrient proxies", "Nutrient sources",
  "Nutrient sources", "Nutrient sources", "Nutrient sources", 
  "Nutrient sources", "Nutrient proxies","Nutrient proxies"),
                      stringsAsFactors = FALSE)

# View(pred_key)
write.csv(pred_key, "data/predictor_key.csv", row.names = FALSE)