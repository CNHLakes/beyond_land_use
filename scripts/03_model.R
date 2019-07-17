# setwd("../")
source("scripts/99_utils.R")

lg <- lagosne_load("1.087.1")
dt        <- readRDS("data/dt_scaled.rds") 
names(dt) <- gsub("_", "v", names(dt))

good_hu4s <- readRDS("data/dt.rds") %>%
  group_by(hu4_zoneid) %>% 
  tally() %>%
  dplyr::filter(n >= 3)
dt <- dplyr::filter(dt, hu4vzoneid %in% good_hu4s$hu4_zoneid)

brm_fit <- function(destfile, formula, data){
  fit <- brm(formula = formula, data = data, prior = set_prior("horseshoe(2)"), 
             family = gaussian(), control = list(adapt_delta = 0.99))
  
  saveRDS(fit, destfile)
  return(fit)
}


# evaulate fixed effects
# ag + {maxdepth, baseflow, iwslavratio}

(model_forms_fe <- list(
  "tp" = bf(tp ~ ag), # proxy
  "tp_depth" = bf(tp ~ ag + maxdepth), # lake
  "tp_bf" = bf(tp ~ ag + maxdepth + hu12vbaseflowvmean), # transport
  "tp_nfert" = bf(tp ~ ag + maxdepth + hu12vbaseflowvmean + 
                    phosphorusvfertilizervuse), # sources
  "tp_buffer" = bf(tp ~ ag + maxdepth + hu12vbaseflowvmean +
                     phosphorusvfertilizervuse + buffervcultivatedvcrops), # buffer
  "tn" = bf(tn ~ ag), # proxy
  "tn_depth" = bf(tn ~ ag + maxdepth), # lake
  "tn_sc" = bf(tn ~ ag + maxdepth + soilvorgvcarbon), # transport
  "tn_nfert" = bf(tn ~ ag + maxdepth + soilvorgvcarbon + 
                    nitrogenvfertilizervuse), # sources 
  "tn_buffer" = bf(tn ~ ag + maxdepth + soilvorgvcarbon + 
                     nitrogenvfertilizervuse + buffervcultivatedvcrops), # buffer
  "tn_ndep" = bf(tn ~ ag + maxdepth + soilvorgvcarbon + 
                   nitrogenvfertilizervuse + buffervcultivatedvcrops + 
                   hu4vnitrogenvatmosphericvdeposition)
))

fe_brms <- 
  lapply(seq_along(model_forms_fe), function(i) 
    get_if_not_exists(brm_fit, 
                      paste0("data/mcmc/fe/", names(model_forms_fe)[i]), 
                      formula = model_forms_fe[[i]], 
                      data = dt))


r2_fe <- dplyr::bind_rows(
  lapply(fe_brms, function(x) data.frame(brms::bayes_R2(x)))) %>%
  mutate(Model = names(model_forms_fe), 
         Estimate = round(Estimate, 2)) %>%
  dplyr::select(Model, Estimate) 

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
                       nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
                       phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural +
                      (1 + ag | hu4vzoneid)),
  "tp_soybeans" = bf(tp ~  
                       maxdepth + iwslavratio +
                       soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                       clayvpct + hu12vbaseflowvmean +
                       nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
                       phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural +
                      (1 + soybeans | hu4vzoneid)),
  "tp_pasture" = bf(tp ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
                      phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural +
                      (1 + pasture | hu4vzoneid)),
  "tn_ag"      = bf(tn ~  maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
                      phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural +
                      (1 + ag | hu4vzoneid)),
  "tn_corn"    = bf(tn ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
                      phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural +
                           (1 + corn | hu4vzoneid)), 
  "tn_pasture" = bf(tn ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
                      phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural +
                      (1 + pasture | hu4vzoneid))
))

re_brms <- list()
for(i in seq_along(model_forms_re)){
  # i <- 2
  print(paste0("Fitting ", paste0("data/mcmc/re/", names(model_forms_re)[i])))
  if(!file.exists(paste0("data/mcmc/re/", names(model_forms_re)[i]))){
    re_brms[[i]] <- brm(formula = model_forms_re[[i]], data = dt, 
                        prior = set_prior("horseshoe(1)"), family = gaussian(), 
                        control = list(adapt_delta = 0.99))
    saveRDS(re_brms[[i]], paste0("data/mcmc/re/", names(model_forms_re)[i]))
  }else{
    re_brms[[i]] <- readRDS(paste0("data/mcmc/re/", names(model_forms_re)[i]))
  }
}

re_brms <- 
  lapply(seq_along(model_forms_re), function(i) 
    get_if_not_exists(brm_fit, 
                      paste0("data/mcmc/re/", names(model_forms_re)[i]), 
                      formula = model_forms_re[[i]], 
                      data = dt))

if(!interactive()){
  # investigate horseshoe prior
  # fit_tn <- re_brms[[5]] # 5 is best tn model with random effects
  # tidybayes::get_variables(fit_tn)
  # model_forms_re[[5]]
  tn_horse_form_all <- bf(tn ~ 
                            maxdepth + iwslavratio +
                            soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                            clayvpct + hu12vbaseflowvmean +
                            nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
                            phosphorusvfertilizervuse + pvinput + phosphorusvlivestockvmanure +
                            buffervcultivatedvcrops + buffervnatural +
                        (1 + ag | hu4vzoneid))
  horse_fit_all <- brm(formula = tn_horse_form_all, data = dt, 
                   prior = set_prior("horseshoe(1)"), family = gaussian(), 
                   control = list(adapt_delta = 0.99))
  test <- brm_fit("data/mcmc/re/test.rds", tn_horse_form_all, data = dt)
  
  summary(horse_fit_all)
  test <- get_re_signif(horse_fit_all)
}


# evaluate hu4 re slope significance
get_re_text <- function(x){
  # x <- "maxdepth + hu12vbaseflowvmean + phosphorusvfertilizervuse + buffervcultivatedvcrops + (1 + ag | hu4vzoneid)"
  res <- strsplit(x, "\\|")[[1]][1]
  res <- strsplit(res, " ")[[1]]
  res[length(res)]
}

get_re_signif <- function(x){
  # x <- re_brms[[1]]
  # tidybayes::get_variables(x)
  print(as.character(x$formula)[1])
  
  re_global <- x %>%
    spread_draws(!!rlang::parse_expr(
      paste0("sd_hu4vzoneid__", get_re_text(as.character(x$formula)[1])))) %>%
    dplyr::select(tail(names(.), 1)) %>%
    pull(names(.)[1]) %>%
    quantile(c(0.05, 0.5, 0.95))
  
  res <- x %>%
    spread_draws(r_hu4vzoneid[hu4vzoneid,term]) %>%
    dplyr::filter(term == get_re_text(as.character(x$formula)[1])) %>%
    group_by(hu4vzoneid) %>%
    do(tibble::as_tibble(t(quantile(.$r_hu4vzoneid, c(0.05, 0.5, 0.95))))) %>%
    mutate(signif = case_when(`5%` > 0 ~ TRUE, 
                              TRUE ~ FALSE))
  x$re            <- res
  x$re_global     <- re_global
  x$re_signif     <- any(res$signif)
  x$re_signif_ids <- dplyr::filter(res, signif == TRUE)$hu4vzoneid
  x
}
  
re_brms <- lapply(re_brms, function(x) get_re_signif(x))

# save model residuals
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

saveRDS(re_brms, "data/mcmc/re_brms.rds")
# unlink("data/mcmc/re_brms.rds")
# re_brms <- readRDS("data/mcmc/re_brms.rds")

r2_re <- dplyr::bind_rows(
  lapply(re_brms, function(x) data.frame(brms::bayes_R2(x)))) %>%
  mutate(Model = names(model_forms_re), 
         Estimate = round(Estimate, 2)) %>%
  dplyr::select(Model, Estimate)
r2_re$re_signif <- unlist(lapply(res_brms, function(x) x$re_signif))

# r2_re$`Proxy`     <- c("Ag", "Soybeans", "Pasture",
#                        "Ag", "Corn", "Pasture") 
# r2_re$`Lake`      <- c(rep("maxdepth", 6))
# r2_re$`Transport` <- c(rep("Baseflow", 3), rep("Soil Org Carbon", 3))
# r2_re$`Source`    <- c(rep("P fertilizer", 3), rep("N fertilizer", 3))
# r2_re$`Buffer`    <- c(rep("Row Crop", 6))

r2    <- dplyr::bind_rows(r2_fe, r2_re)

if(!interactive()){
  write.csv(r2, 
            "data/mcmc/model_r2.csv",
            row.names = FALSE)
}
# r2 <- read.csv("../data/mcmc/model_r2.csv")

# ---- diagnostics ----
if(interactive()){
# loo comparison
# r2_re
loo_compare(loo(re_brms[[4]]), loo(re_brms[[5]]), 
            loo(re_brms[[6]]), loo(re_brms[[7]])) # Corn model has lowest error
loo_compare(loo(re_brms[[1]]), loo(re_brms[[2]]), 
            loo(re_brms[[3]])) # Pasture model has lowest error

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
