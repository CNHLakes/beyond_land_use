# ---- load_data_and_packages
library(magrittr)
library(dplyr)
library(lme4)
library(sf)
library(units)
library(merTools)
library(ggplot2)
library(cowplot)

# setwd("_episodes_rmd")
crops <- readRDS("lagos_ag/data/mi_hu6_corn.rds")
ep    <- readRDS("lagos_ag/data/mi_ep.rds") %>%
  left_join(data.frame(crops), by = c("hu6" = "location_desc"))
ep$hu6_corn_acres <- set_units(as_units(ep$hu6_corn_acres, "acres"), "m2")
ep <- mutate(ep, hu6_corn_pcent = hu6_corn_acres / hu6_area)

# ---- exploratory_plots ----

ggplot(data = ep) +
  geom_point(aes(x = iws_ag_2006_pcent, y = as.numeric(hu6_corn_pcent)))

plot_grid(
ggplot(data = ep) +
  geom_point(aes(x = iws_ag_2006_pcent, y = tp)),
ggplot(data = ep) +
  geom_point(aes(x = as.numeric(hu6_corn_pcent), y = tp))
)

# par(mfrow = c(1, 3))
# plot(ep$hu6_corn_pcent, ep$tp)
# plot(ep$iws_ag_2006_pcent, ep$tp)
# plot(ep$iws_ag_2006_pcent, ep$hu6_corn_pcent)
# abline(0, 1)
# par(mfrow = c(1,1))

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
