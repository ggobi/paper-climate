# This file contains the glyph-map for nasa data Might think about
# putting all of the glyph-map examples, using gistemp and ushcn data
# here too
world <- map_data("world")
world <- getbox(world, xlim = c(-114.93, -55.07), ylim = c(-21.53, 36.64))
# Remove slivvers

world <- ddply(world, "group", function(df) {
  if (diff(range(df$long)) < 1e-6) return(NULL)
  if (diff(range(df$lat)) < 1e-6) return(NULL)
  df
})

map <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = world, legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(breaks = NA, expand = c(0.02, 0)),
  scale_y_continuous(breaks = NA, expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

# De-seasonalized nasa data: matches the facetted, coloured map, need
# to run that code before this
resids.gly <- glyphs(resids, "long", "day", "lat", "temp_resid", height=2.5) 
ggplot(resids.gly, aes(gx, gy, group = gid)) + map +
  ref_lines(resids.gly) +
  ref_boxes(resids.gly) +
  geom_path() + theme_fullframe() + coord_map()
#qplot(gx, gy, data = resids, geom = "line", group = gid) + map
ggsave("../images/nasa-deseas-glyph.png")

# Scatterplot
nasa.scale<-ddply(nasa, c("long","lat"), summarise,
                  cloudlow.s=scale(cloudlow), 
                  cloudhigh.s=scale(cloudhigh),
                  temp.s=scale(surftemp))
#nasa.scat.gly <- glyphs(nasa.scale, "long", "cloudlow.s", "lat", "cloudhigh.s") 
#ggplot(nasa.scat.gly, aes(gx, gy, group = gid)) + map +
#  add_ref_boxes(nasa.scat.gly) +
#  geom_point(size=I(0.7), alpha=I(0.5)) + theme_fullframe() + coord_map()
#ggsave("../images/nasa-scat-glyph.png")
nasa.scat.gly <- glyphs(nasa.scale, "long", "temp.s", "lat", "cloudhigh.s") 
ggplot(nasa.scat.gly, aes(gx, gy, group = gid)) + map +
  add_ref_boxes(nasa.scat.gly) +
  geom_point(size=I(0.7), alpha=I(0.5)) + theme_fullframe() + coord_map()
ggsave("../images/nasa-scat-glyph.png")
nasa.loess<-ddply(nasa.scale, c("long","lat"), summarise,
                  temp.s=temp.s,
                  pcloud=predict(loess(cloudhigh.s~temp.s)))
nasa.loess.gly <- glyphs(nasa.loess, "long", "temp.s", "lat", "pcloud") 
ggplot(nasa.loess.gly, aes(gx, gy, group = gid)) + map +
  add_ref_boxes(nasa.loess.gly) +
  geom_line() + theme_fullframe() + coord_map()
ggsave("../images/nasa-loess-glyph.png")
