# This file contains the glyph-map for nasa data Might think about
# putting all of the glyph-map examples, using gistemp and ushcn data
# here too

source("glyph.r")
source("maps.r")
source("data-nasa.r")
source("glyph-utils.r")

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

ggplot(resids, aes(long, lat)) +
  geom_tile(aes(fill = temp_resid)) +
  outline_nasa + 
  facet_grid(year ~ month) + 
  scale_fill_gradient2(mid = "white", high = "red", low = "blue") + 
  theme_fullframe() +
  coord_map()
ggsave("../images/nasa-colored-map.png", height = 10, width = 18)

# Second figure: same data but with glyphs -----------------------------------

resids.gly <- glyphs(resids, "long", "day", "lat", "temp_resid", height=2.5) 
ggplot(resids.gly, aes(gx, gy, group = gid)) + 
  map_nasa +
  add_ref_lines(resids.gly) +
  add_ref_boxes(resids.gly) +
  geom_path() + 
  theme_fullframe() 
ggsave("../images/nasa-deseas-glyph.png", width = 6, height = 6)

