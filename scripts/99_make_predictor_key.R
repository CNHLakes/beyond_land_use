pred_key <- data.frame(var = c(
  "nitrogen_atmospheric_deposition","maxdepth","iwsla_ratio",
  "hu12_baseflow_mean","hu12_ppt_mean","lake_cultivated_crops", 
  "stream_cultivated_crops","wetlands","ag", "forest",
  "buffer_cultivated_crops", "stream_natural", "buffer_natural",
  "nonag", "nfixer", "clay_pct","lake_area_ha", 
  "wetland_potential", "corn","nitrogen_livestock_manure", 
  "soil_org_carbon", "soybeans","n_input", 
  "nitrogen_fertilizer_use", "phosphorus_fertilizer_use","p_input", 
  "phosphorus_livestock_manure", "wheat","pasture", "row_crop_pct"),
                      pretty = c(
    "N deposition", "Max depth", "Watershed-lake ratio", 
    "Baseflow", "Precipitation", "Lake-buffer Ag", 
    "Stream-buffer Ag", "Wetlands", "Ag", "Forest",
    "Buffer Ag", "Stream-buffer natural", "Buffer natural",
    "Non-ag", "N-fixer", "Clay", "Lake area",
    "Wetland potential", "Corn", "Manure N input", 
    "Soil organic carbon", "Soybeans", "N input",
    "Fertilizer N input", "Fertilizer P input", "P input", 
    "Manure P input", "Wheat", "Pasture", "Row-crop Ag"),
                      category = c(
  "Nutrient inputs", "Lake", "Lake", 
  "Nutrient transport", "Nutrient transport", "Buffer composition",
  "Buffer composition","Land-use cover","Land-use cover", "Land-use cover",
  "Buffer composition", "Buffer composition", "Buffer composition",
  "Land-use cover", "Land-use cover", "Nutrient transport","Lake",
  "Nutrient transport", "Land-use cover","Nutrient inputs", 
  "Nutrient transport", "Land-use cover", "Nutrient inputs",
  "Nutrient inputs", "Nutrient inputs", "Nutrient inputs", 
  "Nutrient inputs", "Land-use cover","Land-use cover", "Land-use cover"),
                      stringsAsFactors = FALSE)
  
pred_key$varv <- gsub("_", "v", pred_key$var)

# View(pred_key)
write.csv(pred_key, "data/predictor_key.csv", row.names = FALSE)