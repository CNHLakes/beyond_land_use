###SMC, 17 Aug 2017
# will be using county level data for extended ag data, so interested in seeing whether LAGOS NCLD ag data at the county scale are correlated to other more relevant spatial scales for watersheds, e.g. IWS, HU12, HU4

# ---- setup -----
library(LAGOSNE)
lg <- lagosne_load(version="1.087.1")

# ---- pull_data ----

geo.relevantzones <- lagosne_select(table = "lakes.geo",
            vars = c("lagoslakeid", "hu4_zoneid", "hu12_zoneid", "county_zoneid"))

county.ag                  <- lagosne_select(table = "county.lulc",
      vars = c("county_zoneid", "county_nlcd2006_pct_81", "county_nlcd2006_pct_82"))
names(county.ag)           <- c("county_zoneid", "county_pasturehay06", "county_rowcrop06")
county.ag$county_totalag06 <- county.ag$county_pasturehay06 + county.ag$county_rowcrop06

#

iws.ag <- lagosne_select(table = "iws.lulc", vars = c("lagoslakeid", "iws_nlcd2006_pct_81",
                                                    "iws_nlcd2006_pct_82"))
names(iws.ag) <- c("lagoslakeid", "iws_pasturehay06", "iws_rowcrop06")
iws.ag$iws_totalag06 <- iws.ag$iws_pasturehay06 + iws.ag$iws_rowcrop06

#

ag.county <- merge(geo.relevantzones, county.ag, by = "county_zoneid", all.x = T,
                   all.y = T)
ag.county.iws <- merge(ag.county, iws.ag, by = "lagoslakeid", all.x = T, all.y = T)
ag.county.iws <- na.omit(ag.county.iws)

# doesn't look so great, but maybe ok?  add in hu12 and hu4 to see how everything
# looks against each other and whether it's just county that sucks
hu12.ag                <- lagosne_select(table = "hu12.lulc",
                                         vars = c("hu12_zoneid", "hu12_nlcd2006_pct_81", "hu12_nlcd2006_pct_82"))
names(hu12.ag)         <- c("hu12_zoneid", "hu12_pasturehay06", "hu12_rowcrop06")
hu12.ag$hu12_totalag06 <- hu12.ag$hu12_pasturehay06 + hu12.ag$hu12_rowcrop06

hu4.ag               <- lagosne_select(table = "hu4.lulc",
                                       vars = c("hu4_zoneid", "hu4_nlcd2006_pct_81", "hu4_nlcd2006_pct_82"))
names(hu4.ag)        <- c("hu4_zoneid", "hu4_pasturehay06", "hu4_rowcrop06")
hu4.ag$hu4_totalag06 <- hu4.ag$hu4_pasturehay06 + hu4.ag$hu4_rowcrop06

ag.county.iws.hu12     <- merge(ag.county.iws, hu12.ag, by = "hu12_zoneid",
                                all.x = T, all.y = T)
ag.county.iws.hu12.hu4 <- merge(ag.county.iws.hu12, hu4.ag, by = "hu4_zoneid",
                                all.x = T, all.y = T)
ag.county.iws.hu12.hu4 <- na.omit(ag.county.iws.hu12.hu4)

# ---- viz_county_vs_iws ----
# is county equivalent to IWS?
plot(ag.county.iws$iws_totalag06 ~ ag.county.iws$county_totalag06,
     xlab = "County % Total Ag", ylab = "IWS % Total Ag")

plot(ag.county.iws$iws_rowcrop06 ~ ag.county.iws$county_rowcrop06,
     xlab = "County % Row Crop Ag", ylab = "IWS % Row Crop Ag")

plot(ag.county.iws$iws_pasturehay06 ~ ag.county.iws$county_pasturehay06,
     xlab = "County % Pasture Ag", ylab = "IWS % Pasture Ag")

# ---- all_ag_combos_panel ----
# plot all combos of total ag vs each other
par(mfrow = c(3, 2), mar = c(0, 0, 0, 0), oma = c(4, 4, 1, 1), xpd = NA)

plot(ag.county.iws.hu12.hu4$iws_totalag06 ~ ag.county.iws.hu12.hu4$county_totalag06,
     cex.axis = 1.2, xaxt = "n", xlab = "", ylab = "")
text(8, 8, "IWS:CO", col = "white", cex = 2)
# text(-9, 50, 'IWS % Ag', srt=90, cex=1.3)

plot(ag.county.iws.hu12.hu4$iws_totalag06 ~ ag.county.iws.hu12.hu4$hu12_totalag06,
     cex.axis = 1.2, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
text(8, 8, "IWS:HU12", col = "white", cex = 2)
# text(-5, 50, 'IWS % Ag', srt=90, cex=1.3)

plot(ag.county.iws.hu12.hu4$hu12_totalag06 ~ ag.county.iws.hu12.hu4$county_totalag06,
     ylab = "", xlab = "", xaxt = "n", cex.axis = 1.2)
text(8, 8, "HU12:CO", col = "white", cex = 2)
# text(-9, 50, 'HU12 % Ag', srt=90, cex=1.3)

plot(ag.county.iws.hu12.hu4$hu4_totalag06 ~ ag.county.iws.hu12.hu4$hu12_totalag06,
     ylab = "", xlab = "", cex.axis = 1.2)
text(90, 8, "HU4:HU12", cex = 2)
# text(-8, 40, 'HU4 % Ag', srt=90, cex=1.3) text(50, 83, 'HU12 % Ag', cex=1.3)

plot(ag.county.iws.hu12.hu4$hu4_totalag06 ~ ag.county.iws.hu12.hu4$county_totalag06,
     ylab = "", xlab = "", cex.axis = 1.2)
text(90, 6, "HU4:CO", cex = 2)
# text(-9, 42, 'HU4 % Ag', srt=90, cex=1.3) text(50, -12, 'County % Ag (all 3
# panels)', cex=1.3)

plot(ag.county.iws.hu12.hu4$hu4_totalag06 ~ ag.county.iws.hu12.hu4$iws_totalag06,
     yaxt = "n", ylab = "", xlab = "", cex.axis = 1.2)
text(90, 6, "HU4:IWS", cex = 2)
