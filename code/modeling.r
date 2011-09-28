library("ggplot2")
library("plyr")
library("mgcv")

source("data-nasa.r")
source("glyph.r")
source("glyph-utils.r")
source("cache.r")
source("maps.r")

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

# Rescale predictions to individual scales -----------------------------------

day_preds <- glyphs(day_preds, "long", "day", "lat", "pred") 
ggplot(day_preds, aes(gx, gy, group = gid)) + 
  map_nasa + 
  add_ref_boxes(day_preds) +
  geom_path() +
  theme_fullframe()
ggsave("../images/month-rescale-none.png", width = 4, height = 4)

last_plot() %+% glyphs(day_preds, "long", "day", "lat", "pred", 
  y_scale = range01) 
ggsave("../images/month-rescale01.png", width = 4, height = 4)

day_preds2 <- ddply(day_preds, c("lat", "long"), mutate, 
  pred_s = rescale01(pred),
  pred_m = pred / max(pred),
  max = max(pred),
  range = diff(range(pred))
)

day_preds2 <- glyphs(day_preds2, "long", "day", "lat", "pred_s") 
ggplot(day_preds2, aes(gx, gy, group = gid)) + 
  map + 
  geom_path(aes(colour = range)) +
  ref_boxes +
  theme_fullframe() + 
  scale_colour_gradient(high = "black", low = "grey60")
ggsave("../images/month-rescale01-col.png", width = 4, height = 4)

grid <- unique(day_preds2[c("lat", "long", "range")])

ggplot(day_preds2) + 
  map + 
  geom_tile(aes(long, lat, fill = range), data = grid, alpha = 0.5) +
  geom_path(aes(gx, gy, group = gid)) +
  theme_fullframe() + 
  scale_fill_gradient(low = "white", high = "#3B4FB8")
ggsave("../images/month-rescale01-fill.png", width = 4, height = 4)



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

# Scatterplots ----------------------------------------------------------------

cloud <- glyphs(nasa, "long", "cloudlow", "lat", "cloudhigh") 
ggplot(cloud, aes(gx, gy, group = gid)) + 
  map +
  geom_point(shape = ".") +
  geom_tile(aes(long, lat), colour = "white", fill = NA)
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
