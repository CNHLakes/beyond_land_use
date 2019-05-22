source("scripts/99_utils.R")

library(brms)
library(tidybayes)
library(modelr)

dt        <- readRDS("data/dt.rds") %>% 
  dplyr::select("lagoslakeid", "hu4_zoneid", "hu12_zoneid", 
                "tp", "tn", "hu12_ppt_mean", "hu12_baseflow_mean",
                "maxdepth", "iwsla_ratio", "ag", "row_crop_pct", "corn", 
                "soybeans", "pasture", "p_input", "n_input",
                "phosphorus_fertilizer_use", "nitrogen_fertilizer_use", 
                "stream_cultivated_crops")
names(dt) <- gsub("_", "v", names(dt))
dt        <- dplyr::filter(dt, 
                           !is.na(phosphorusvfertilizervuse), 
                           !is.na(soybeans), 
                           !is.na(corn),
                           !is.na(maxdepth), 
                           !is.na(tp), 
                           !is.na(tn))
dt <- mutate_at(dt, scale, .vars = vars(-lagoslakeid, -hu4vzoneid, -hu12vzoneid))

good_hu4s <- readRDS("data/dt.rds") %>%
  group_by(hu4_zoneid) %>% 
  tally() %>%
  dplyr::filter(n >= 3)
dt <- dplyr::filter(dt, hu4vzoneid %in% good_hu4s$hu4_zoneid)

brm_fit <- function(destfile, formula, data){
  fit <- brm(formula = formula, data = data, 
             family = gaussian())
  saveRDS(fit, destfile)
  return(fit)
}


# evaulate fixed effects
# ag + {maxdepth, baseflow, iwslavratio}

(model_forms <- list(
  "tp_depth+bf" = bf(tp ~ ag + maxdepth + hu12vbaseflowvmean),
  "tp_depth+iwsla" = bf(tp ~ ag + maxdepth + iwslavratio), 
  "tp_fe" = bf(tp ~ ag + maxdepth + hu12vbaseflowvmean + iwslavratio), 
  "tn_depth+bf" = bf(tn ~ ag + maxdepth + hu12vbaseflowvmean),
  "tn_depth+iwsla" = bf(tn ~ ag + maxdepth + iwslavratio), 
  "tn_fe" = bf(tn ~ ag + maxdepth + hu12vbaseflowvmean + iwslavratio) 
))

fe_brms <- 
  lapply(seq_along(model_forms), function(i) 
    get_if_not_exists(brm_fit, 
                      paste0("data/mcmc/fe/", names(model_forms)[i]), 
                      formula = model_forms[[i]], 
                      data = dt))


r2_fe <- dplyr::bind_rows(
  lapply(fe_brms, function(x) data.frame(brms::bayes_R2(x)))) %>%
  mutate(Model = names(model_forms), 
         Estimate = round(Estimate, 2)) %>%
  dplyr::select(Model, Estimate) 


# evaulate spatial random effects
# {fixed effects} + 1/ag, 1/soybeans, 1/corn
(model_forms <- list(
  "tp_ag" = bf(tp ~  maxdepth + hu12vbaseflowvmean +
                 (1 + ag | hu4vzoneid)),
  "tp_soybeans" = bf(tp ~  maxdepth + hu12vbaseflowvmean +
                       (1 + soybeans | hu4vzoneid)),
  "tp_re" = bf(tp ~  maxdepth + hu12vbaseflowvmean +
                 (1 + ag + soybeans | hu4vzoneid)), 
  "tn_ag" = bf(tn ~  maxdepth + hu12vbaseflowvmean +
                 (1 + ag | hu4vzoneid)),
  "tn_corn" = bf(tn ~  maxdepth + hu12vbaseflowvmean +
                       (1 + corn | hu4vzoneid)),
  "tn_re" = bf(tn ~  maxdepth + hu12vbaseflowvmean +
                 (1 + ag + corn | hu4vzoneid))
))


re_brms <- 
  lapply(seq_along(model_forms), function(i) 
    get_if_not_exists(brm_fit, 
                      paste0("data/mcmc/re/", names(model_forms)[i]), 
                      formula = model_forms[[i]], 
                      data = dt))


r2_re <- dplyr::bind_rows(
  lapply(re_brms, function(x) data.frame(brms::bayes_R2(x)))) %>%
  mutate(Model = names(model_forms), 
         Estimate = round(Estimate, 2)) %>%
  dplyr::select(Model, Estimate)

if(!interactive()){
  write.csv(dplyr::bind_rows(r2_fe, r2_re), 
            "data/mcmc/model_r2.csv",
            row.names = FALSE)
}

# ---- diagnostics ----

# dotplot of model residuals
lg <- LAGOSNE::lagosne_load()
test <- dt %>% 
  add_residual_draws(re_brms[[5]]) %>%
  left_join(dplyr::select(lg$locus, lagoslakeid, nhd_lat, nhd_long), 
            by = "lagoslakeid") %>%
  group_by(lagoslakeid) %>%
  summarize(.residual_median = median(.residual))
  LAGOSNE::coordinatize() %>%

ggplot() + 
  geom_sf(data = test, aes(color = .residual))


# autocorrelation plot of model residuals
dt %>%
  add_residual_draws(fe_brms[[1]])


# look for evidence of interaction effects
# following shalizi...
dt %>%
  add_residual_draws(fe_brms[[1]]) %>%
  ggplot(aes(x = log(maxdepth + abs(min(dt$maxdepth))), y = .residual, color = iwslavratio)) +
  geom_point() +
  # stat_pointinterval(alpha = 0.4) +
  theme(axis.text.x = element_text(angle = 90))

# ----

# knitr::kable(r2_fe, "markdown")
# knitr::kable(r2_re, "markdown")
# 
# fe_brms[[1]]$formula
# re_brms[[5]]$formula
# 
# dt %>%
#   add_residual_draws(fe_brms[[1]]) %>%
#   ggplot(aes(x = maxdepth, y = .residual)) +
#   stat_pointinterval() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# pairs(~maxdepth + hu12vbaseflowvmean + iwslavratio + , data = dt)

# ---- placeholder ----

# cat("predictors:
# lake: depth
# iws: area, ws:lk,
#      pasture, wheat, corn, soybean, non-ag, other ag,
#      org c, hydro conduc, runoff pot, erod,
#      N fert, P fert, manure, N dep
# reponse:
#   tn, tp")

# ---- exploratory_models ----
# #### Find optimal random effects structure
# library(lme4)
# library(ggeffects)

# model_form <- as.formula(tp ~ 1 + ag + (1 + ag | hu4vzoneid))
# fit0 <- lmer(model_form, data = dt, na.action = na.omit)
# 
# model_form <- as.formula(tp ~ 1 + streamvcultivatedvcrops + 
#                            (1 + streamvcultivatedvcrops | hu4vzoneid))
# fit1 <- lmer(model_form, data = dt, na.action = na.omit)
# 
# sjstats::r2(fit0); sjstats::r2(fit1);
# 
# model_form <- as.formula(tp ~ 1 + 
#                            streamvcultivatedvcrops + maxdepth + 
#   (1 + streamvcultivatedvcrops | hu4vzoneid))
# fit2 <- lmer(model_form, data = dt, na.action = na.omit)
# 
# # #### Fit unconditional model
# formula_unconditional <- as.formula(tp ~ (1|hu4_zoneid) - 1)
# fit <- lmer(formula_unconditional,
#             data = dt, na.action = na.omit)
# getME(fit, "X")
# getME(fit, "Z")
#  
# re <- ggpredict(fit, type = "re")
# sjstats::r2(fit)

# #####
# 
# dt     <- readRDS("data/dt.rds")
# 
# #### explore mediation effects
# # https://en.wikipedia.org/wiki/Mediation_(statistics)
# fit1 <- lm(tn ~ row_crop_pct, data = dt)
# fit2 <- lm(tn ~ maxdepth, data = dt)
# fit3 <- lm(tn ~ row_crop_pct + maxdepth, data = dt)
# 
# summary(fit1)
# summary(fit2)
# summary(fit3)
# 
# ####
#
# plot(re)$hu4_zoneid + 
#   geom_hline(aes(yintercept = mean(
#     dplyr::filter(dt, hu4_zoneid == "HU4_16")$tp))) +
#   theme(axis.text.x = element_text(angle = 90))
# 
# # shinyMer(fit)
# 
# fit_ranef <- left_join(
#   data.frame(ranef(fit), stringsAsFactors = FALSE), 
#   dplyr::select(dt, hu4_zoneid, geometry), 
#   by = c("grp" = "hu4_zoneid"))
# st_geometry(fit_ranef) <- fit_ranef$geometry
# fit_ranef <- mutate(fit_ranef, 
#                     grp_id = stringr::str_extract(grp, "(?<=_)(\\d*)$"))
# 
# # mapview::mapview(fit_ranef, zcol = "condval")
# 
# dt_mean      <- mean(dt$tp, na.rm = TRUE)
# 
# test <- data.frame(dt) %>%
#   group_by(hu4_zoneid) %>%
#   summarize(tp_grp = mean(tp, na.rm = TRUE),
#             tp_diff = tp_grp - dt_mean) %>%
#   left_join(
#     dplyr::select(data.frame(fit_ranef), grp, condval), 
#     by = c("hu4_zoneid" = "grp")) %>%
#   distinct()
# 
# mean(test$tp_grp) 
# 
# plot(test$tp_grp, test$condval)
# plot(test$tp_diff, test$condval)
# abline(0, 1)
# abline(h = dt_mean)
# 
# #### Fit random intercept model with covariate model
# fit <- lmer(tp ~ 1 + iws_ag_2006_pcent + (1|hu6), data=dt, na.action=na.omit)
# 
# #### Fit random intercept and slope with covariate
# fit <- lmer(tp ~ 1 + iws_ag_2006_pcent + (1+iws_ag_2006_pcent|hu6), data=dt, na.action=na.omit)
# 
# # shinyMer(fit)
# 
# fit_ranef <- left_join(data.frame(ranef(fit)),
#                        dplyr::select(crops, location_desc, geom),
#                        by = c("grp" = "location_desc"))
# 
# # mapview(st_sf(fit_ranef), zcol = "condval")
