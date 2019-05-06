source("scripts/99_utils.R")

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

library(dplyr)
library(lme4)
library(merTools)
library(LAGOSNE)
library(sf)
library(ggeffects)
library(ggplot2)

dt     <- readRDS("data/dt.rds")

# table(
#   cbind(row.names(dt_tp), row.names(dt_tn))[1:15,]
#   )

# cor.test(dt_sub$tn, dt_sub$corn)

dt <- coordinatize(dt)
dt <- data.frame(dt) %>%
  mutate(hu4_zoneid = as.factor(hu4_zoneid)) %>%
  group_by(hu4_zoneid) %>%
  mutate(tp_grp = mean(tp, na.rm = TRUE),
         tp_diff = tp_grp - dt_mean) 

#### explore mediation effects
# https://en.wikipedia.org/wiki/Mediation_(statistics)
fit1 <- lm(tn ~ row_crop_pct, data = dt)
fit2 <- lm(tn ~ iwsla_ratio, data = dt)
fit3 <- lm(tn ~ row_crop_pct + iwsla_ratio, data = dt)

summary(fit1)
summary(fit3)


#### Fit unconditional model
formula_unconditional <- as.formula(tp ~ (1|hu4_zoneid) - 1)
fit <- lmer(formula_unconditional, 
            data = dt, na.action = na.omit)
getME(fit, "X")
getME(fit, "Z")

re <- ggpredict(fit, type = "re")
# sjstats::r2(fit)
plot(re)$hu4_zoneid + 
  geom_hline(aes(yintercept = mean(
    dplyr::filter(dt, hu4_zoneid == "HU4_16")$tp))) +
  theme(axis.text.x = element_text(angle = 90))

# shinyMer(fit)

fit_ranef <- left_join(
  data.frame(ranef(fit), stringsAsFactors = FALSE), 
  dplyr::select(dt, hu4_zoneid, geometry), 
  by = c("grp" = "hu4_zoneid"))
st_geometry(fit_ranef) <- fit_ranef$geometry
fit_ranef <- mutate(fit_ranef, 
                    grp_id = stringr::str_extract(grp, "(?<=_)(\\d*)$"))

# mapview::mapview(fit_ranef, zcol = "condval")

dt_mean      <- mean(dt$tp, na.rm = TRUE)

test <- data.frame(dt) %>%
  group_by(hu4_zoneid) %>%
  summarize(tp_grp = mean(tp, na.rm = TRUE),
            tp_diff = tp_grp - dt_mean) %>%
  left_join(
    dplyr::select(data.frame(fit_ranef), grp, condval), 
    by = c("hu4_zoneid" = "grp")) %>%
  distinct()

mean(test$tp_grp) 

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
