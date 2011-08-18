source("0-nasa.r")
source("0-glyphs.r")

library(ggplot2)
library(plyr)
library(mgcv)

space <- dlply(nasa, "time", function(df) { 
   gam(surftemp ~ s(lat, long), data =df)
})

loc_grid <- unique(nasa[c("lat", "long")])

space_preds <- ldply(space, function(mod) {
  loc_grid$pred <- predict(mod, newdata = loc_grid)
  loc_grid
})

space_preds <- glyphs(space_preds, "long", "time", "lat", "pred")
qplot(gx, gy, data = space_preds, geom = "line", group = gid) + map

qplot(long, lat, data = space_preds, fill = pred, geom = "tile") + facet_wrap(~ time)


big_mod <- gam(surftemp ~ s(long, lat, time) + factor(month), data = nasa)
loc_grid <- expand.grid(
  lat = unique(nasa$lat), 
  long = unique(nasa$long), 
  month = 1, 
  time = seq(0, 2161, length = 50))
loc_grid$pred <- predict(big_mod, newdata = loc_grid)

loc_grid <- glyphs(loc_grid, "long", "time", "lat", "pred")
qplot(gx, gy, data = loc_grid, geom = "line", group = gid) + map


nasa$resid <- resid(big_mod)
nasa <- glyphs(nasa, "long", "time", "lat", "resid")
qplot(gx, gy, data = nasa, geom = "line", group = gid) + map
