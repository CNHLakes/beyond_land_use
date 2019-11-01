# setwd("../")
source("scripts/99_utils.R")

lg        <- lagosne_load("1.087.1")
dt_raw    <- readRDS("data/dt.rds")
dt        <- readRDS("data/dt_scaled.rds") 
names(dt) <- gsub("_", "v", names(dt))

good_hu4s <- readRDS("data/dt.rds") %>%
  group_by(hu4_zoneid) %>% 
  tally() %>%
  dplyr::filter(n >= 3)
dt <- dplyr::filter(dt, hu4vzoneid %in% good_hu4s$hu4_zoneid)
high_ag_llids <- dplyr::filter(dt_raw, ag > 40) %>%
  pull(lagoslakeid)
dt <- dplyr::filter(dt, lagoslakeid %in% high_ag_llids)

(model_forms_re <- list(
  "tp_ag"       = bf(tp ~  
                       maxdepth + iwslavratio +
                       soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                       clayvpct + hu12vbaseflowvmean +
                       nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                       phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural +
                       (1 + ag | hu4vzoneid)),
  "tp_forest"       = bf(tp ~  
                           maxdepth + iwslavratio +
                           soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                           clayvpct + hu12vbaseflowvmean +
                           nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                           phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                           buffervcultivatedvcrops + buffervnatural +
                           (1 + forest | hu4vzoneid)),
  "tp_wetlands"       = bf(tp ~  
                             maxdepth + iwslavratio +
                             soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                             clayvpct + hu12vbaseflowvmean +
                             nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                             phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                             buffervcultivatedvcrops + buffervnatural +
                             (1 + wetlands | hu4vzoneid)),
  "tp_rowcrop"       = bf(tp ~  
                            maxdepth + iwslavratio +
                            soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                            clayvpct + hu12vbaseflowvmean +
                            nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                            phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                            buffervcultivatedvcrops + buffervnatural +
                            (1 + rowvcropvpct | hu4vzoneid)),
  "tp_pasture" = bf(tp ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural +
                      (1 + pasture | hu4vzoneid)),
  "tp_soybeans" = bf(tp ~  
                       maxdepth + iwslavratio +
                       soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                       clayvpct + hu12vbaseflowvmean +
                       nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                       hu4vnitrogenvatmosphericvdeposition +
                       phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                       buffervcultivatedvcrops + buffervnatural +
                       (1 + soybeans | hu4vzoneid)),
  "tp_corn" = bf(tp ~  
                   maxdepth + iwslavratio +
                   soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                   clayvpct + hu12vbaseflowvmean +
                   nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                   hu4vnitrogenvatmosphericvdeposition +
                   phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                   buffervcultivatedvcrops + buffervnatural +
                   (1 + corn | hu4vzoneid)),
  "tn_ag"      = bf(tn ~  maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural +
                      (1 + ag | hu4vzoneid)),
  "tn_forest"      = bf(tn ~  maxdepth + iwslavratio +
                          soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                          clayvpct + hu12vbaseflowvmean +
                          nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                          hu4vnitrogenvatmosphericvdeposition +
                          phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                          buffervcultivatedvcrops + buffervnatural +
                          (1 + forest | hu4vzoneid)),
  "tn_wetlands"       = bf(tn ~  
                             maxdepth + iwslavratio +
                             soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                             clayvpct + hu12vbaseflowvmean +
                             nitrogenvfertilizervuse + nitrogenvlivestockvmanure + hu4vnitrogenvatmosphericvdeposition +
                             phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                             buffervcultivatedvcrops + buffervnatural +
                             (1 + wetlands | hu4vzoneid)),
  "tn_rowcrop"      = bf(tn ~  maxdepth + iwslavratio +
                           soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                           clayvpct + hu12vbaseflowvmean +
                           nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                           hu4vnitrogenvatmosphericvdeposition +
                           phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                           buffervcultivatedvcrops + buffervnatural +
                           (1 + rowvcropvpct | hu4vzoneid)),
  "tn_pasture" = bf(tn ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural +
                      (1 + pasture | hu4vzoneid)),
  "tn_corn"    = bf(tn ~  
                      maxdepth + iwslavratio +
                      soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                      clayvpct + hu12vbaseflowvmean +
                      nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                      hu4vnitrogenvatmosphericvdeposition +
                      phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                      buffervcultivatedvcrops + buffervnatural +
                      (1 + corn | hu4vzoneid)),
  "tn_soybeans"    = bf(tn ~  
                          maxdepth + iwslavratio +
                          soilvorgvcarbon + wetlandvpotential + hu12vpptvmean + 
                          clayvpct + hu12vbaseflowvmean +
                          nitrogenvfertilizervuse + nitrogenvlivestockvmanure +
                          hu4vnitrogenvatmosphericvdeposition +
                          phosphorusvfertilizervuse + phosphorusvlivestockvmanure +
                          buffervcultivatedvcrops + buffervnatural +
                          (1 + soybeans | hu4vzoneid))
))

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

saveRDS(re_brms, "data/mcmc/re_brms_40.rds")