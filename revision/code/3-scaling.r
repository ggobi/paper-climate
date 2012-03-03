library("ggplot2")
library("plyr")
library("mgcv")

source("models-nasa")
source("glyph.r")
source("maps.r")

# Rescale predictions to individual scales -----------------------------------

day_preds <- glyphs(day_preds, "long", "day", "lat", "pred") 
ggplot(day_preds, aes(gx, gy, group = gid)) + 
  map_nasa + 
  add_ref_boxes(day_preds) +
  geom_path() +
  theme_fullframe()
ggsave("../images/month-rescale-none.png", width = 4, height = 4)

ggplot(day_preds, aes(day, pred)) + 
  geom_line(aes(group = gid), alpha = 1/10) + 
  opts(aspect.ratio = 1) + 
  xlab("Days from start") + 
  ylab("Average temperature (K)")
ggsave("../images/month-rescale-legend.pdf", width = 4, height = 4)

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
  map_nasa + 
  add_ref_boxes(day_preds) +
  geom_path(aes(colour = range)) +
  theme_fullframe() + 
  scale_colour_gradient("Temperature\nrange (K)",
    high = "black", low = "grey60", limits = c(0, 8.5),
    breaks = seq(0, 8, by = 2), guide = guide_colourbar(
      direction = "horizontal", title.vjust = 0.7, 
      title.theme = theme_text(face = "bold"))) +
  opts(legend.position = "bottom", aspect.ratio = 1) 
ggsave("../images/month-rescale01-col.png", width = 4, height = 4.5)

grid <- unique(day_preds2[c("lat", "long", "range")])

ggplot(day_preds2) + 
  map_nasa + 
  geom_tile(aes(long, lat, fill = range), data = grid, alpha = 0.5) +
  geom_path(aes(gx, gy, group = gid)) +
  theme_fullframe() + 
  scale_fill_gradient("Temperature\nrange (K)",
    high = "white", low = "#3B4FB8", limits = c(0, 8.5),
    breaks = seq(0, 8, by = 2), guide = guide_colourbar(
      direction = "horizontal", title.vjust = 0.7, 
      title.theme = theme_text(face = "bold"))) +
  opts(legend.position = "bottom", aspect.ratio = 1) 
ggsave("../images/month-rescale01-fill.png", width = 4, height = 4.5)



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
