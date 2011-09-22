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
resids.gly <- glyphs(resids, "long", "day", "lat", "temp_resid") 
colnames(resids.gly)[6] <- "lon"
ggplot(resids.gly, aes(gx, gy, group = gid)) + map + geom_line(aes(y = lat), colour = "white", size = 1.5) + geom_path() + ref_boxes + theme_fullframe() + coord_map()
#qplot(gx, gy, data = resids, geom = "line", group = gid) + map
ggsave("../images/nasa-deseas-glyph.png")
