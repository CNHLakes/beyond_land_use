# ---- placeholder ----

cat("predictors:
lake: depth
iws: area, ws:lk,
      pasture, wheat, corn, soybean, non-ag, other ag,
      org c, hydro conduc, runoff pot, erod,
      N fert, P fert, manure, N dep
reponse:
  tn, tp")

# ---- exploratory_models ----

#### Fit unconditional model
fit <- lmer(tp ~ 1 + (1|hu6), data=ep, na.action=na.omit)

#### Fit random intercept model with covariate model
fit <- lmer(tp ~ 1 + iws_ag_2006_pcent + (1|hu6), data=ep, na.action=na.omit)

#### Fit random intercept and slope with covariate
fit <- lmer(tp ~ 1 + iws_ag_2006_pcent + (1+iws_ag_2006_pcent|hu6), data=ep, na.action=na.omit)

# shinyMer(fit)

fit_ranef <- left_join(data.frame(ranef(fit)),
                       dplyr::select(crops, location_desc, geom),
                       by = c("grp" = "location_desc"))

# mapview(st_sf(fit_ranef), zcol = "condval")
