# This file contains the glyph-map for nasa data Might think about
# putting all of the glyph-map examples, using gistemp and ushcn data
# here too

source("maps.r")
# world, map are created in maps.r


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
