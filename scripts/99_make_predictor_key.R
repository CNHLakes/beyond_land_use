suppressMessages(library(dplyr))

pred_key <- data.frame(var = c(
  "nitrogen_atmospheric_deposition","maxdepth","iwsla_ratio",
  "hu12_baseflow_mean","hu12_ppt_mean",
  "stream_cultivated_crops","wetlands","ag", "forest", "rowvcropvpct",
  "buffer_cultivated_crops", "buffer_natural",
  "nonag", "nfixer", "clay_pct","lake_area_ha", 
  "wetland_potential", "corn","nitrogen_livestock_manure", 
  "soil_org_carbon", "soybeans","n_input", 
  "nitrogen_fertilizer_use", "phosphorus_fertilizer_use","p_input", 
  "hu4vnitrogenvatmosphericvdeposition",
  "phosphorus_livestock_manure", "wheat","pasture", "row_crop_pct"),
                      pretty = c(
    "N deposition", "Max depth", "Watershed-lake ratio", 
    "Baseflow", "Precipitation", 
    "Stream-buffer Ag", "Wetlands", "Ag", "Forest", "Row-crop",
    "Buffer Ag", "Buffer natural",
    "Non-ag", "N-fixer", "Clay", "Lake area",
    "Wetland potential", "Corn", "Manure N", 
    "Soil organic carbon", "Soybeans", "N input",
    "Fertilizer N", "Fertilizer P", "P input", 
    "N Deposition",
    "Manure P", "Wheat", "Pasture", "Row-crop Ag"),
  granularity = c("Other", "Other", "Other", 
                  "Other", "Other", 
                  "Granular", "Other", "Aggregate", "Other", "Aggregate",
                  "Granular", "Granular", 
                  "Aggregate", "Granular", "Other", "Other", 
                  "Other", "Granular", "Granular", 
                  "Other", "Granular", "Granular", 
                  "Granular", "Granular", "Granular",
                  "Granular", 
                  "Granular", "Granular", "Aggregate", "Aggregate"),
                      category = c(
  "Nutrient inputs", "Lake", "Lake", 
  "Nutrient transport", "Nutrient transport", 
  "Buffer configuration","Land-use cover","Land-use cover", "Land-use cover", "Land-use cover",
  "Buffer configuration", "Buffer configuration",
  "Land-use cover", "Land-use cover", "Nutrient transport","Lake",
  "Nutrient transport", "Land-use cover","Nutrient inputs", 
  "Nutrient transport", "Land-use cover", "Nutrient inputs",
  "Nutrient inputs", "Nutrient inputs", "Nutrient inputs", 
  "Nutrient inputs",
  "Nutrient inputs", "Land-use cover","Land-use cover", "Land-use cover"),
                      stringsAsFactors = FALSE)
  
pred_key$varv <- gsub("_", "v", pred_key$var)

pred_key <- dplyr::filter(pred_key, 
                          !(pretty %in% c("Lake area", "N-fixer", 
                                          "Non-ag")))

# View(pred_key)
write.csv(pred_key, "data/predictor_key.csv", row.names = FALSE)