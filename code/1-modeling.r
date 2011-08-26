library("ggplot2")
library("plyr")
library("mgcv")

source("0-glyphs.r")
source("0-nasa.r")
source("0-cache.r")

# Scatteplots ----------------------------------------------------------------

temp <- glyphs(nasa, "long", "temperature", "lat", "surftemp") 
ggplot(temp, aes(gx, gy, group = gid)) + 
  map +
  geom_point(shape = ".")

cloud <- glyphs(nasa, "long", "cloudlow", "lat", "cloudhigh") 
ggplot(cloud, aes(gx, gy, group = gid)) + 
  map +
  geom_point(shape = ".")
ggsave("../images/clouds.png", width = 6, height = 6)

grid_along <- function(x, n = 20) {
  rng <- range(x, na.rm = TRUE)
  seq(rng[1], rng[2], length = n)
}

cloud_mod <- ddply(nasa, c("lat", "long"), function(df) {
  grid_low <- data.frame(cloudlow = grid_along(df$cloudlow))
  mod <- loess(cloudhigh ~ cloudlow, data = df, na = na.exclude)
  mutate(grid_low, cloudhigh = predict(mod, newdata = grid_low))
})
cloud_mod <- glyphs(cloud_mod, "long", "cloudlow", "lat", "cloudhigh") 

ggplot(cloud_mod, aes(gx, gy, group = gid)) + 
  map +
  geom_line() +
  geom_tile(aes(long, lat), colour = "white", fill = NA) 
ggsave("../images/clouds-smooth.png", width = 6, height = 6)


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



# Coefficients ---------------------------------------------------------------

coefs <- ldply(temp_models, function(x) {
  data.frame(month = 2:12, coef = coef(x)[-(1:2)])
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


# Reference lines ------------------------------------------------------------

month_preds <- glyphs(month_preds, "long", "month", "lat", "pred") 
ggplot(month_preds, aes(gx, gy, group = gid)) +
  map +
  geom_line(aes(y = lat), colour = "white", size = 1.5) +
  geom_line()
ggsave("../images/ref-line.pdf", width = 6, height = 6)

ggplot(month_preds, aes(gx, gy, group = gid)) +
  map +
  geom_line() +
  geom_tile(aes(long, lat), colour = "white", fill = NA)
ggsave("../images/ref-box.pdf", width = 6, height = 6)

maxes <- ddply(month_preds, "gid", subset, order(-gy) == 1)
ggplot(month_preds, aes(gx, gy, group = gid)) +
  map +
  geom_tile(aes(long, lat), colour = "white", fill = NA) +
  geom_line() +
  geom_point(aes(colour = month), data = maxes, size = 1.5)
ggsave("../images/ref-max-1.pdf", width = 6, height = 6)

ggplot(month_preds, aes(gx, gy, group = gid)) +
  map +
  geom_tile(aes(long, lat, fill = month), data = maxes, colour = "white",
    alpha = 0.5) +
  geom_line()
ggsave("../images/ref-max-2.pdf", width = 6, height = 6)


# Cartesian vs. polar --------------------------------------------------------

month_preds <- glyphs(month_preds, "long", "month", "lat", "pred") 
ggplot(month_preds, aes(gx, gy, group = gid)) + 
  map_mini + 
  geom_path() +
  geom_tile(aes(long, lat), colour = "white", fill = NA)
ggsave("../images/month-cartesian.pdf", width = 4, height = 4)

month_preds <- glyphs(month_preds, "long", "month", "lat", "pred", polar = T) 
ggplot(month_preds, aes(gx, gy, group = gid)) + 
  map_mini + 
  geom_path() +
  geom_tile(aes(long, lat), colour = "white", fill = NA)
ggsave("../images/month-polar.pdf", width = 4, height = 4)

# Rescale predictions to individual scales -----------------------------------
day_preds2 <- ddply(day_preds, c("lat", "long"), mutate, 
  pred_s = rescale01(pred),
  pred_m = pred / max(pred))
day_preds2 <- glyphs(day_preds2, "long", "day", "lat", "pred_s") 

ggplot(day_preds2, aes(gx, gy, group = gid)) + 
  map + 
  geom_path() +
  geom_tile(aes(long, lat), colour = "white", fill = NA)
ggsave("../images/month-rescale01.pdf", width = 4, height = 4)

day_preds2 <- glyphs(day_preds2, "long", "day", "lat", "pred_m") 
ggplot(day_preds2, aes(gx, gy, group = gid)) + 
  map + 
  geom_path() +
  geom_tile(aes(long, lat), colour = "white", fill = NA)
ggsave("../images/month-rescale-max.pdf", width = 4, height = 4)

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


# Shrink the pdfs without losing any detail
tools::compactPDF("../images/")