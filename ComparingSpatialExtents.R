###SMC, 17 Aug 2017
#will be using county level data for extended ag data, so interested in seeing whether LAGOS NCLD ag data at the county scale are correlated to other more relevant spatial scales for watersheds, e.g. IWS, HU12, HU4

#install LAGOS package bc haven't used it before
devtools::install_github("cont-limno/LAGOS", update_dependencies = TRUE)

library(LAGOS)
lagos_get("1.087.1")
data<-lagos_load(version="1.087.1")

geo.relevantzones<-lagos_select(table="lakes.geo", vars=c("lagoslakeid", "hu4_zoneid", "hu12_zoneid","county_zoneid"))

county.ag<-lagos_select(table="county.lulc", vars=c("county_zoneid", "county_nlcd2006_pct_81", "county_nlcd2006_pct_82"))
names(county.ag)<-c("county_zoneid", "county_pasturehay06", "county_rowcrop06")
county.ag$county_totalag06<-county.ag$county_pasturehay06+county.ag$county_rowcrop06

iws.ag<-lagos_select(table="iws.lulc", vars=c("lagoslakeid", "iws_nlcd2006_pct_81", "iws_nlcd2006_pct_82"))
names(iws.ag)<-c("lagoslakeid", "iws_pasturehay06", "iws_rowcrop06")
iws.ag$iws_totalag06<-iws.ag$iws_pasturehay06+iws.ag$iws_rowcrop06

ag.county<-merge(geo.relevantzones, county.ag, by="county_zoneid", all.x=T, all.y=T)
ag.county.iws<-merge(ag.county, iws.ag, by="lagoslakeid", all.x=T, all.y=T)
ag.county.iws<-na.omit(ag.county.iws)

#is county equivalent to IWS?
plot(ag.county.iws$iws_totalag06~ag.county.iws$county_totalag06, xlab="County % Total Ag", ylab="IWS % Total Ag")
plot(ag.county.iws$iws_rowcrop06~ag.county.iws$county_rowcrop06, xlab="County % Row Crop Ag", ylab="IWS % Row Crop Ag")
plot(ag.county.iws$iws_pasturehay06~ag.county.iws$county_pasturehay06, xlab="County % Pasture Ag", ylab="IWS % Pasture Ag")
