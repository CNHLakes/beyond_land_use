###SMC, 17 Aug 2017
#will be using county level data for extended ag data, so interested in seeing whether LAGOS NCLD ag data at the county scale are correlated to other more relevant spatial scales for watersheds, e.g. IWS, HU12, HU4

#
devtools::install_github("cont-limno/LAGOS", update_dependencies = TRUE)


library(LAGOS)
lagos_get("1.087.1")

