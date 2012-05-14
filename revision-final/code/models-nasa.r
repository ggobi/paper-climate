library("plyr")
library("mgcv")
source("cache.r")
source("data-nasa.r")

# Model fitting --------------------------------------------------------------

temp_models %<-cache% dlply(nasa, c("lat", "long"), function(df) {
  lm(surftemp ~ year + factor(month), data = df)
})

temp_smooth %<-cache% dlply(nasa, c("lat", "long"), function(df) {
  gam(surftemp ~ s(day) + factor(month), data = df)
})

month_grid <- expand.grid(year = 2000, month = 1:12)
month_preds <- ldply(temp_models, function(mod) {
  month_grid$pred <- predict(mod, newdata = month_grid)
  month_grid
})

year_grid <- expand.grid(year = unique(nasa$year), month = 1)
year_preds <- ldply(temp_models, function(mod) {
  year_grid$pred <- predict(mod, newdata = year_grid)
  year_grid
})

day_grid <- expand.grid(day = seq(0, 2161, length = 50), month = 1)
day_preds <- ldply(temp_smooth, function(mod) {
  day_grid$pred <- predict(mod, newdata = day_grid)
  day_grid
})
