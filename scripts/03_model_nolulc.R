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
(model_forms_fe <- list(
  "tp_nolulc" = bf(tp ~ maxdepth + iwslavratio +
                     soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                     clayvpct + hu12vbaseflowvmean +
                     nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                     hu4vnitrogenvatmosphericvdeposition +
                     phosphorusvfertilizervuse +
                     phosphorusvlivestockvmanure),
  "tn_nolulc" = bf(tn ~ maxdepth + iwslavratio +
                     soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                     clayvpct + hu12vbaseflowvmean +
                     nitrogenvfertilizervuse + nvinput + nitrogenvlivestockvmanure +
                     hu4vnitrogenvatmosphericvdeposition +
                     phosphorusvfertilizervuse + pvinput + 
                     phosphorusvlivestockvmanure)
))

fe_brms <- 
  lapply(seq_along(model_forms_fe), function(i) 
    get_if_not_exists(brm_fit, 
                      paste0("data/mcmc/fe_nolulc/", names(model_forms_fe)[i]), 
                      formula = model_forms_fe[[i]], 
                      data = dt))


# save model residuals
re_brms <- lapply(fe_brms, function(x) get_residuals(x))

# get r2
re_brms <- lapply(fe_brms, function(x) get_r2(x))

# get loo
re_brms <- lapply(fe_brms, function(x){
  x$loo <- loo(x, model_names = get_re_text(as.character(x$formula)[[1]])); x})

saveRDS(fe_brms, "data/mcmc/fe_brms_nolulc.rds")
