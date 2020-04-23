# setwd("../")
source("scripts/99_utils.R")

lg        <- lagosne_load("1.087.1")
dt        <- readRDS("data/dt_scaled.rds") 
names(dt) <- gsub("_", "v", names(dt))

good_hu4s <- readRDS("data/dt.rds") %>%
  group_by(hu4_zoneid) %>% 
  tally() %>%
  dplyr::filter(n >= 3)
dt <- dplyr::filter(dt, hu4vzoneid %in% good_hu4s$hu4_zoneid)

# evaulate fixed effects
# (model_forms_fe <- list(
#   "tp" = bf(tp ~ ag),
#   "tp_nolulc" = bf(tp ~ maxdepth + iwslavratio +
#                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
#                      clayvpct + hu12vbaseflowvmean +
#                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
#                      hu4vnitrogenvatmosphericvdeposition +
#                      phosphorusvfertilizervuse +
#                      phosphorusvlivestockvmanure),
#   "tp_nuts" = bf(tp ~ nitrogenvfertilizervuse + nvinput + 
#                      nitrogenvlivestockvmanure +
#                      hu4vnitrogenvatmosphericvdeposition +
#                      phosphorusvfertilizervuse + pvinput + 
#                      phosphorusvlivestockvmanure),
#   "tp_depth" = bf(tp ~ ag + maxdepth), # lake
#   "tp_bf" = bf(tp ~ ag + maxdepth + hu12vbaseflowvmean), # transport
#   "tp_nfert" = bf(tp ~ ag + maxdepth + hu12vbaseflowvmean + 
#                     phosphorusvfertilizervuse), # sources
#   "tp_buffer" = bf(tp ~ ag + maxdepth + hu12vbaseflowvmean +
#                      phosphorusvfertilizervuse + buffervcultivatedvcrops), # buffer
#   "tn" = bf(tn ~ ag), # proxy
#   "tn_nolulc" = bf(tn ~ maxdepth + iwslavratio +
#                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
#                      clayvpct + hu12vbaseflowvmean +
#                      nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
#                      hu4vnitrogenvatmosphericvdeposition +
#                      phosphorusvfertilizervuse + pvinput + 
#                      phosphorusvlivestockvmanure),
#   "tn_depth" = bf(tn ~ ag + maxdepth), # lake
#   "tn_sc" = bf(tn ~ ag + maxdepth + soilvorgvcarbon), # transport
#   "tn_nfert" = bf(tn ~ ag + maxdepth + soilvorgvcarbon + 
#                     nitrogenvfertilizervuse), # sources 
#   "tn_buffer" = bf(tn ~ ag + maxdepth + soilvorgvcarbon + 
#                      nitrogenvfertilizervuse + buffervcultivatedvcrops), # buffer
#   "tn_ndep" = bf(tn ~ ag + maxdepth + soilvorgvcarbon + 
#                    nitrogenvfertilizervuse + buffervcultivatedvcrops + 
#                    hu4vnitrogenvatmosphericvdeposition)
# ))
# 
# fe_brms <- list()
# i <- 3
# print(paste0("Fitting ", paste0("data/mcmc/fe/", names(model_forms_fe)[i])))
# fe_brms[[i]] <- brm(formula = model_forms_fe[[i]], data = dt,
#                       prior = set_prior("horseshoe(2)"), family = gaussian(),
#                       control = list(adapt_delta = 0.99))
# summary(fe_brms[[i]])

#   saveRDS(re_brms[[i]], paste0("data/mcmc/re/", names(model_forms_re)[i]))
# }# else{
#   re_brms[[i]] <- readRDS(paste0("data/mcmc/re/", names(model_forms_re)[i]))
# }
# }

# fe_brms <- 
#   lapply(seq_along(model_forms_fe), function(i) 
#     get_if_not_exists(brm_fit, 
#                       paste0("data/mcmc/fe/", names(model_forms_fe)[i]), 
#                       formula = model_forms_fe[[i]], 
#                       data = dt))

# r2_fe <- dplyr::bind_rows(
#   lapply(fe_brms, function(x) data.frame(brms::bayes_R2(x)))) %>%
#   mutate(Model = names(model_forms_fe), 
#          Estimate = round(Estimate, 2)) %>%
#   dplyr::select(Model, Estimate) 

# r2_fe$`Proxy`     <- c(rep("Ag", 5), 
#                        rep("Ag", 5))
# r2_fe$`Lake`      <- c(NA, rep("maxdepth", 4), 
#                        NA, rep("maxdepth", 4))
# r2_fe$`Transport` <- c(NA, NA, "Baseflow", "Baseflow", "Baseflow",
#                        NA, NA, "Soil Org Carbon", "Soil Org Carbon", "Soil Org Carbon")
# r2_fe$`Source`    <- c(NA, NA, NA, "P fertilizer", "P fertilizer",
#                        NA, NA, NA, "N fertilizer", "N fertilizer")
# r2_fe$`Buffer`    <- c(NA, NA, NA, NA, "Row crop",
#                        NA, NA, NA, NA, "Row crop")

# evaulate spatial random effects
# {fixed effects} + 1/ag, 1/soybeans, 1/corn
(model_forms_re <- list(
  "tp_ag"       = bf(tp ~  
                       maxdepth + iwslavratio +
                       soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                       clayvpct + hu12vbaseflowvmean +
                       nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                       phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural + ag +
                      (1 + ag | hu4vzoneid)),
  "tp_forest"       = bf(tp ~  
                       maxdepth + iwslavratio +
                       soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                       clayvpct + hu12vbaseflowvmean +
                       nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                       phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural + forest +
                       (1 + forest | hu4vzoneid)),
  "tp_wetlands"       = bf(tp ~  
                           maxdepth + iwslavratio +
                           soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                           clayvpct + hu12vbaseflowvmean +
                           nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                           phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                           buffervcultivatedvcrops + buffervnatural + wetlands +
                           (1 + wetlands | hu4vzoneid)),
  "tp_rowcrop"       = bf(tp ~  
                       maxdepth + iwslavratio +
                       soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                       clayvpct + hu12vbaseflowvmean +
                       nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                       phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural + rowvcropvpct +
                       (1 + rowvcropvpct | hu4vzoneid)),
  "tp_pasture" = bf(tp ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural + pasture +
                      (1 + pasture | hu4vzoneid)),
  "tp_soybeans" = bf(tp ~  
                       maxdepth + iwslavratio +
                       soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                       clayvpct + hu12vbaseflowvmean +
                       nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                       hu4vnitrogenvatmosphericvdeposition +
                       phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural + soybeans +
                      (1 + soybeans | hu4vzoneid)),
  "tp_corn" = bf(tp ~  
                       maxdepth + iwslavratio +
                       soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                       clayvpct + hu12vbaseflowvmean +
                       nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                       hu4vnitrogenvatmosphericvdeposition +
                       phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural + corn + 
                       (1 + corn | hu4vzoneid)),
  "tn_ag"      = bf(tn ~  maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural + ag +
                      (1 + ag | hu4vzoneid)),
  "tn_forest"      = bf(tn ~  maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural + forest +
                      (1 + forest | hu4vzoneid)),
  "tn_wetlands"       = bf(tn ~  
                             maxdepth + iwslavratio +
                             soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                             clayvpct + hu12vbaseflowvmean +
                             nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                             phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                             buffervcultivatedvcrops + buffervnatural + wetlands +
                             (1 + wetlands | hu4vzoneid)),
  "tn_rowcrop"      = bf(tn ~  maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural + rowvcropvpct +
                      (1 + rowvcropvpct | hu4vzoneid)),
  "tn_pasture" = bf(tn ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural + pasture +
                      (1 + pasture | hu4vzoneid)),
  "tn_corn"    = bf(tn ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural + corn +
                           (1 + corn | hu4vzoneid)),
  "tn_soybeans"    = bf(tn ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural + soybeans +
                      (1 + soybeans | hu4vzoneid))
))

# re_brms <- list()
# for(i in seq_along(model_forms_re)){
# i <- 7# 13
# print(paste0("Fitting ", paste0("data/mcmc/re/", names(model_forms_re)[i])))
# if(!file.exists(paste0("data/mcmc/re/", names(model_forms_re)[i]))){
#   re_brms[[i]] <- brm(formula = model_forms_re[[i]], data = dt,
#                       prior = set_prior("horseshoe(1)"), family = gaussian(),
#                       control = list(adapt_delta = 0.99))
#   saveRDS(re_brms[[i]], paste0("data/mcmc/re/", names(model_forms_re)[i]))
# }# else{
#   re_brms[[i]] <- readRDS(paste0("data/mcmc/re/", names(model_forms_re)[i]))
# }
# }

re_brms <- 
  lapply(seq_along(model_forms_re), function(i) 
    get_if_not_exists(brm_fit, 
                      paste0("data/mcmc/re/", names(model_forms_re)[i]), 
                      formula = model_forms_re[[i]], 
                      data = dt))

# (model_forms_resuper <- list("tn_crops"    = bf(tn ~  
#                                                      maxdepth + iwslavratio +
#                                                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
#                                                      clayvpct + hu12vbaseflowvmean +
#                                                      nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
#                                                      hu4vnitrogenvatmosphericvdeposition +
#                                                      phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
#                                                      buffervcultivatedvcrops + buffervnatural +
#                                                      (1 + soybeans + corn + pasture | hu4vzoneid)),
#                              "tn_ag"    = bf(tn ~  
#                                                   maxdepth + iwslavratio +
#                                                   soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
#                                                   clayvpct + hu12vbaseflowvmean +
#                                                   nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
#                                                   hu4vnitrogenvatmosphericvdeposition +
#                                                   phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
#                                                   buffervcultivatedvcrops + buffervnatural +
#                                                   (1 + ag | hu4vzoneid)),
#                              "tp_crops"    = bf(tp ~  
#                                                   maxdepth + iwslavratio +
#                                                   soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
#                                                   clayvpct + hu12vbaseflowvmean +
#                                                   nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
#                                                   hu4vnitrogenvatmosphericvdeposition +
#                                                   phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
#                                                   buffervcultivatedvcrops + buffervnatural +
#                                                   (1 + soybeans + corn + pasture | hu4vzoneid)), 
#                              "tp_ag"    = bf(tp ~  
#                                                   maxdepth + iwslavratio +
#                                                   soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
#                                                   clayvpct + hu12vbaseflowvmean +
#                                                   nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
#                                                   hu4vnitrogenvatmosphericvdeposition +
#                                                   phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
#                                                   buffervcultivatedvcrops + buffervnatural +
#                                                   (1 + ag | hu4vzoneid))
#                              ))
# 
# resuper_brms <- 
#   lapply(seq_along(model_forms_resuper), function(i) 
#     get_if_not_exists(brm_fit, 
#                       paste0("data/mcmc/resuper/", names(model_forms_resuper)[i]), 
#                       formula = model_forms_resuper[[i]], 
#                       data = dt))

# evaluate hu4 re slope significance
re_brms <- lapply(re_brms, function(x) get_re_signif(x))

# save model residuals
re_brms <- lapply(re_brms, function(x) get_residuals(x))

# get r2
re_brms <- lapply(re_brms, function(x) get_r2(x))

# get loo
re_brms <- lapply(re_brms, function(x){
  x$loo <- loo(x, model_names = get_re_text(as.character(x$formula)[[1]])); x})

saveRDS(re_brms, "data/mcmc/re_brms.rds")
# unlink("data/mcmc/re_brms.rds")
# re_brms <- readRDS("data/mcmc/re_brms.rds")

# ---- diagnostics ----
if(interactive()){
# loo comparison
# r2_re
loo_compare(loo(re_brms[[4]]), loo(re_brms[[5]]), 
            loo(re_brms[[6]])) # Corn model has lowest error
loo_compare(loo(re_brms[[1]]), loo(re_brms[[2]]), 
            loo(re_brms[[3]], reloo = TRUE)) # Ag model has lowest error

# evaluate hu4 re slope significance
corn_hu4_dist <- re_brms[[5]] %>%
  spread_draws(r_hu4vzoneid[hu4vzoneid,term]) %>%
  dplyr::filter(term == "corn") %>%
  group_by(hu4vzoneid) %>%
  do(tibble::as_tibble(t(quantile(.$r_hu4vzoneid, c(0.05, 0.5, 0.95))))) %>%
  mutate(signif = case_when(`5%` > 0 ~ TRUE, 
                            TRUE ~ FALSE))

pasture_hu4_dist <- re_brms[[3]] %>%
  spread_draws(r_hu4vzoneid[hu4vzoneid,term]) %>%
  dplyr::filter(term == "pasture") %>%
  group_by(hu4vzoneid) %>%
  do(tibble::as_tibble(t(quantile(.$r_hu4vzoneid, c(0.05, 0.5, 0.95))))) %>%
  mutate(signif = case_when(`5%` > 0 ~ TRUE, 
                            TRUE ~ FALSE))

# sum(corn_hu4_dist$signif)
# sum(pasture_hu4_dist$signif)

ggplot(data = corn_hu4_dist) +
  geom_pointrange(aes(x = hu4vzoneid, y = `50%`, 
                      ymin = `5%`, ymax = `95%`, color = signif)) +
  geom_hline(yintercept = 0) +
  coord_flip()

hu4 <-  LAGOSNEgis::query_gis("HU4", "ZoneID", unique(dt$hu4vzoneid)) %>%
  st_cast(to = "MULTIPOLYGON") 
hu4_signif <-  LAGOSNEgis::query_gis(
  "HU4", "ZoneID", dplyr::filter(corn_hu4_dist, signif)$hu4vzoneid) %>% 
  st_cast(to = "MULTIPOLYGON")

# mapview::mapview(hu4) + 
# mapview::mapview(hu4_signif, color = "red")


# get median residuals of each model object
get_residuals <- function(model, threshold = 0.1){
  lg <- parent.env(environment())$lg
  # model <- re_brms[[1]]
  model$res_med <- dt %>%
    add_residual_draws(model) %>%
    group_by(lagoslakeid) %>%
    summarize(.residual_median = median(.residual)) %>%
    left_join(dplyr::select(lg$locus, lagoslakeid, nhd_lat, nhd_long),
              by = "lagoslakeid")

  model$res_med <- dt %>%
    add_fitted_draws(model) %>%
    group_by(lagoslakeid) %>%
    summarize(.value_median = median(.value)) %>%
    right_join(model$res_med, by = "lagoslakeid")

  model$res_test <- abs(
    median(model$res_med$.residual_median, na.rm = TRUE)) < threshold
  model
}
re_brms <- lapply(re_brms, function(x) get_residuals(x))
# 
# # qq plots etc
par(mfrow = c(2, 3))
lapply(re_brms, function(x) hist(x$res_med$.residual_median))
lapply(re_brms, function(x) plot(x$res_med$.value_median,
                                 x$res_med$.residual_median))
lapply(re_brms, function(x){
  qqnorm(x$res_med$.residual_median)
  abline(0, 1)
  })
par(mfrow = c(1,1))
#   
# # dotplot of model residuals
mapview::mapview(LAGOSNE::coordinatize(re_brms[[1]]$res_med),
                 zcol = ".residual_median")
# 
# # get residual spatial autocorrelation range for each model object
# 
# # autocorrelation plot of model residuals
res_med <- re_brms[[1]]$res_med
res_med <- dplyr::filter(res_med, !is.na(.residual_median))
coords <- res_med[,c("nhd_long", "nhd_lat")]
coords <- mutate_all(coords, function(x) abs(as.integer(x * 10)))
names(coords) <- c("x", "y")
ac <- spind::acfft(data.frame(coords),
                   res_med$.residual_median,
                   dmax = 30)
plot(ac)
# # each index increment is equal to a tenth of a degree
# # so 20 is 2 degrees
# 
# # look for evidence of interaction effects
# # following shalizi 2019...
re_brms[[1]]$formula
re_brms[[1]]$res_med %>%
  left_join(dt, by = "lagoslakeid") %>%
  ggplot(aes(x = nvinput, y = .residual_median)) +
  geom_point()
# 
# dt %>%
#   add_residual_draws(fe_brms[[1]]) %>%
#   ggplot(aes(x = log(maxdepth + abs(min(dt$maxdepth))), y = .residual, color = iwslavratio)) +
#   geom_point() +
#   # stat_pointinterval(alpha = 0.4) +
#   theme(axis.text.x = element_text(angle = 90))

# ----

# knitr::kable(r2_fe, "markdown")
# knitr::kable(r2_re, "markdown")
# 
# fe_brms[[1]]$formula
# re_brms[[5]]$formula
# 
dt %>%
  add_residual_draws(fe_brms[[1]]) %>%
  ggplot(aes(x = maxdepth, y = .residual)) +
  stat_pointinterval() +
  theme(axis.text.x = element_text(angle = 90))
# 
# pairs(~maxdepth + hu12vbaseflowvmean + iwslavratio + , data = dt)
}

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
