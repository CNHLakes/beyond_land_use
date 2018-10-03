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
# lme4 cheatsheet - 
library(dplyr)
library(lme4)
library(merTools)
library(LAGOSNE)
library(sf)

dt <- readRDS("data/dt.rds")
dt <- coordinatize(dt)

#### Fit unconditional model
fit <- lmer(tp ~ 1 + (1|hu4_zoneid), 
            data = dt, na.action = na.omit)

# shinyMer(fit)

fit_ranef <- left_join(
  data.frame(ranef(fit), stringsAsFactors = FALSE), 
  dplyr::select(dt, hu4_zoneid, geometry), 
  by = c("grp" = "hu4_zoneid"))
st_geometry(fit_ranef) <- fit_ranef$geometry
fit_ranef <- mutate(fit_ranef, 
                    grp_id = stringr::str_extract(grp, "(?<=_)(\\d*)$"))

# mapview::mapview(fit_ranef, zcol = "condval")

dt_mean <- mean(dt$tp, na.rm = TRUE)

test <- data.frame(dt) %>%
  group_by(hu4_zoneid) %>%
  summarize(tp_grp = mean(tp, na.rm = TRUE),
            tp_diff = tp_grp - dt_mean) %>%
  left_join(
    dplyr::select(data.frame(fit_ranef), grp, condval), 
    by = c("hu4_zoneid" = "grp")) %>%
  distinct()
 
plot(test$tp_grp, test$condval)
plot(test$tp_diff, test$condval)
abline(0, 1)
abline(h = dt_mean)

#### Fit random intercept model with covariate model
fit <- lmer(tp ~ 1 + iws_ag_2006_pcent + (1|hu6), data=dt, na.action=na.omit)

#### Fit random intercept and slope with covariate
fit <- lmer(tp ~ 1 + iws_ag_2006_pcent + (1+iws_ag_2006_pcent|hu6), data=dt, na.action=na.omit)

# shinyMer(fit)

fit_ranef <- left_join(data.frame(ranef(fit)),
                       dplyr::select(crops, location_desc, geom),
                       by = c("grp" = "location_desc"))

# mapview(st_sf(fit_ranef), zcol = "condval")
