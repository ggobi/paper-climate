library("ggplot2")
library("plyr")
library("mgcv")

source("0-glyphs.r")
source("0-nasa.r")

# Coefficients ---------------------------------------------------------------

temp_models <- dlply(nasa, c("lat", "long"), function(df) {
  # lm(ozone ~ factor(month) - 1, data = df)
  lm(surftemp ~ factor(month), data = df)
})


coefs <- ldply(temp_models, function(x) {
  data.frame(month = 2:12, coef = coef(x)[-1])
})

# Show seasonal patterns
coefs <- glyphs(coefs, "long", "month", "lat", "coef") 
qplot(gx, gy, data = coefs, geom = "line", group = gid) + map 
qplot(gx, gy, data = coefs, geom = "line", group = gid) + 
  geom_path(aes(y = lat), colour = "grey50", alpha = 0.75)

coefs <- glyphs(coefs, "long", "month", "lat", "coef", polar = T) 
qplot(gx, gy, data = coefs, geom = "path", group = gid) + map 
ggplot(coefs, aes(gx, gy, group = gid)) +
  map + 
  geom_point(aes(long, lat), colour = "red", shape = ".") + 
  geom_path()

# Predictions ----------------------------------------------------------------

temp_models <- dlply(nasa, c("lat", "long"), function(df) {
  lm(surftemp ~ year + factor(month), data = df)
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

month_preds <- glyphs(month_preds, "long", "month", "lat", "pred") 
year_preds <- glyphs(year_preds, "long", "year", "lat", "pred") 

qplot(gx, gy, data = month_preds, geom = "path", group = gid) + map 
qplot(gx, gy, data = year_preds, geom = "path", group = gid) + map 

temp_smooth <- dlply(nasa, c("lat", "long"), function(df) {
  gam(surftemp ~ s(day) + factor(month), data = df)
}, .progress = "text")
day_grid <- expand.grid(day = seq(0, 2161, length = 50), month = 1)
day_preds <- ldply(temp_smooth, function(mod) {
  day_grid$pred <- predict(mod, newdata = day_grid)
  day_grid
})
day_preds <- glyphs(day_preds, "long", "day", "lat", "pred") 
qplot(gx, gy, data = day_preds, geom = "path", group = gid) + map 

# Rescale predictions to individual scales
day_preds2 <- ddply(day_preds, c("lat", "long"), mutate, 
  pred_s = rescale01(pred))
day_preds2 <- glyphs(day_preds2, "long", "day", "lat", "pred_s") 
qplot(gx, gy, data = day_preds2, geom = "path", group = gid) + map 


# Linear trend is not a good fit!

# Residuals ------------------------------------------------------------------

locs <- dlply(nasa, c("lat", "long"))

resids <- mdply(cbind(d = locs, m = temp_models), function(d, m) {
  d$temp_resid <- d$surftemp - predict(m, newdata = d)
  d
})

resids <- glyphs(resids, "long", "day", "lat", "temp_resid") 
qplot(gx, gy, data = resids, geom = "line", group = gid) + map


# Summary statistics ---------------------------------------------------------

rsq <- function(mod) summary(mod)$r.squared
rsqs <- ldply(temp_models, rsq)

qplot(long, lat, data = rsqs, fill = V1, geom = "tile") + map

last_plot() + geom_line(aes(gx, gy, group = gid, fill = NULL), data = resids)
