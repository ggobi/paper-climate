# Generalisations: scatterplots of arbitrary variables.
source("glyph.r")
source("glyph-utils.r")
source("maps.r")
source("data-nasa.r")

nasa.scale <- ddply(nasa, c("long","lat"), summarise,
  cloudlow.s = scale(cloudlow), 
  cloudhigh.s = scale(cloudhigh),
  temp.s = scale(surftemp))

nasa.scat.gly <- glyphs(nasa.scale, "long", "temp.s", "lat", "cloudhigh.s") 
ggplot(nasa.scat.gly, aes(gx, gy, group = gid)) + 
  map_nasa +
  add_ref_boxes(nasa.scat.gly) +
  geom_point(size = 0.7, alpha = 0.5) + 
  theme_fullframe() 
ggsave("../images/nasa-scat-glyph.png", width = 6, height = 6)

ggplot(nasa, aes(surftemp, cloudhigh)) + 
  geom_point(size = 0.7, alpha = 1 / 10) + 
  ylab("High cloud") + 
  xlab("Surface temperature (K)")
ggsave("../images/nasa-scat-legend.png", width = 4, height = 4)

nasa.loess <- ddply(nasa.scale, c("long","lat"), summarise,
  temp.s = temp.s,
  pcloud = predict(loess(cloudhigh.s ~ temp.s)))
nasa.loess.gly <- glyphs(nasa.loess, "long", "temp.s", "lat", "pcloud") 
ggplot(nasa.loess.gly, aes(gx, gy, group = gid)) + 
  map_nasa +
  add_ref_boxes(nasa.loess.gly) +
  geom_line() + theme_fullframe() 
ggsave("../images/nasa-loess-glyph.png", width = 6, height = 6)

nasa <- ddply(nasa, c("long","lat"), transform,
  pcloud = predict(loess(cloudhigh ~ surftemp)))
ggplot(nasa, aes(surftemp, pcloud)) + 
  geom_line(aes(group = interaction(long, lat)), alpha = 1/10) + 
  ylab("Smoothed high cloud") + 
  xlab("Surface temperature (K)")
ggsave("../images/nasa-loess-legend.png", width = 4, height = 4)
