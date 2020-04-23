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
