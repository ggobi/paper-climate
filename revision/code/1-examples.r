# This file contains the glyph-map for nasa data Might think about
# putting all of the glyph-map examples, using gistemp and ushcn data
# here too

source("glyph.r")
source("glyph-utils.r")
source("maps.r")
source("data-nasa.r")

temp_seas_models <- dlply(nasa, c("lat", "long"), function(df) {
  lm(surftemp ~ factor(month), data = df)
})

# Including trend
temp_trend_models <- dlply(nasa, c("lat", "long"), function(df) {
  lm(surftemp ~ year + factor(month), data = df)
})

locs <- dlply(nasa, c("lat", "long"))

resids <- mdply(cbind(d = locs, m = temp_seas_models), function(d, m) {
  d$temp_resid <- d$surftemp - predict(m, newdata = d)
  d
})

preds <- mdply(cbind(d = locs, m = temp_seas_models), function(d, m) {
  d$temp_pred <- predict(m, newdata = d)
  d
})

# First figure: facetted colored map -----------------------------------------

outline_nasa <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = .nasa, show_guide = FALSE, fill = NA, colour = "grey60"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

# This plot is INCREDIBLY slow in 0.8.9, so I've switched to 0.9.0 to make
# it a bit faster - it's still not great, but it's at least tolerable.

ggplot(resids, aes(long, lat)) +
  geom_raster(aes(fill = temp_resid)) +
  outline_nasa + 
  facet_grid(year ~ month) + 
  scale_fill_gradient2(mid = "white", high = "red", low = "blue") + 
  theme_fullframe() + 
  opts(
    axis.text.x = theme_blank(), 
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), 
    axis.ticks.margin = unit(0, "lines"),
    legend.position = "right")
ggsave("../images/nasa-colored-map.png", height = 10, width = 18, dpi = 72)

# Second figure: same data but with glyphs -----------------------------------

resids.gly <- glyphs(resids, "long", "day", "lat", "temp_resid", height=2.5) 
ggplot(resids.gly, aes(gx, gy, group = gid)) + 
  map_nasa +
  add_ref_lines(resids.gly) +
  add_ref_boxes(resids.gly) +
  geom_path() + 
  theme_fullframe() 
ggsave("../images/nasa-deseas-glyph.png", width = 6, height = 6, dpi = 72)

# Legend
ggplot(resids.gly, aes(time, temp_resid)) + 
  geom_line(aes(group = gid), alpha = 1/10) + 
  opts(aspect.ratio = 1) + 
  xlab("Month") + 
  ylab("Temperature deviation (C)")
ggsave("../images/nasa-deseas-glyph-leg.png", width = 4, height = 4, dpi = 72)
