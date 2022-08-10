library(dplyr)
library(ggplot2)
library(sf)
library(cowplot)

signif_hu4_ids <- c("HU4_56", "HU4_57")
dt <- readRDS("../data/dt.rds") %>%
  mutate(signif_hu4 = hu4_zoneid %in% signif_hu4_ids) %>%
  dplyr::select(lagoslakeid, hu4_zoneid, clay_pct, signif_hu4) %>%
  group_by(hu4_zoneid) %>%
  mutate(median = median(clay_pct, na.rm = TRUE)) %>%
  distinct(hu4_zoneid, median, signif_hu4)

test  <- readRDS("../data/tile_drainage.rds")
test2 <- left_join(test, dt, by = c("ZoneID" = "hu4_zoneid"))

# scatter plot clay versus tile drainage highlighting sensitve hucs
gg_hist <- ggplot(data = test2, aes(tile_drainage / 900 * 100 # grid cells are 30x30 = 900
)) +
  geom_histogram(binwidth = 5, aes(fill = signif_hu4), size = 0.5, color = "black") +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  xlab("Tile drainage (percent)") + ylab("Count (n)") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)) +
  labs(color = "LULC Sensitive")

ggsave("test.png", gg_hist, height = 2, width = 4.3)
