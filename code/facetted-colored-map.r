# This file contains the code for first plot in the paper
# The facetted, colored map
nasa <- readRDS(file.choose()) # nasadata.rds

library("maps")
library("ggplot2")

outlines <- as.data.frame(map("world", plot = FALSE,
  xlim = -c(113.8, 56.2), ylim = c(-21.2, 36.2))[c("x","y")])
map <- geom_path(aes(x, y), inherit.aes = FALSE, data = outlines, 
  alpha = 0.2, legend = FALSE)

# Fit models
# Seasonal only
temp_seas_models <- dlply(nasa, c("lat", "long"), function(df) {
  # lm(ozone ~ factor(month) - 1, data = df)
  lm(surftemp ~ factor(month), data = df)
})

# Including trend
temp_trend_models <- dlply(nasa, c("lat", "long"), function(df) {
  lm(surftemp ~ year + factor(month), data = df)
})

locs <- dlply(nasa, c("lat", "long"))

resids <- mdply(cbind(d = locs, m = temp_seas_models), function(d, m) {
  d$temp_resid <- d$surftemp - predict(m, newdata = d)
  d
})

preds <- mdply(cbind(d = locs, m = temp_seas_models), function(d, m) {
  d$temp_pred <- predict(m, newdata = d)
  d
})

theme_nothing <- function (base_size = 12) {
  structure(list(
	panel.grid.minor=theme_blank(), 
	panel.grid.major=theme_blank(), 
	panel.background=theme_blank(), 
	panel.border = theme_blank(), 
	panel.margin = unit(0, "lines"), 
	axis.title.x=theme_blank(), 
	axis.title.y=theme_blank(), 
	axis.line=theme_blank(), 
	axis.ticks=theme_blank(), 
	axis.text.y = theme_blank(), 
	axis.text.x = theme_blank()#,
	#legend.position = "none"
  ), class = "options")
}
qplot(long, lat, data=resids, geom="tile", fill=temp_resid, facets=year~month) + scale_fill_gradient2(midpoint=0, mid="white", high="red", low="blue") + map + theme_nothing() + coord_map()
ggsave("../images/nasa-colored-map.png", height=10, width=18)
