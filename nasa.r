setwd("/Users/dicook/papers/ClimateChange")
nasa <- readRDS(file.choose()) # nasadata.rds

library("maps")
library("ggplot2")

outlines <- as.data.frame(map("world", plot = FALSE,
  xlim = -c(113.8, 56.2), ylim = c(-21.2, 36.2))[c("x","y")])
map <- geom_path(aes(x, y), inherit.aes = FALSE, data = outlines, 
  alpha = 0.2, legend = FALSE)

# Raw data
qplot(long, lat, data=nasa, geom="tile", fill=surftemp, facets=year~month) + scale_fill_gradient2(midpoint=296.9, mid="white") + map

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

resids <- mdply(cbind(d = locs, m = temp_models), function(d, m) {
  d$temp_resid <- d$surftemp - predict(m, newdata = d)
  d
})

preds <- mdply(cbind(d = locs, m = temp_models), function(d, m) {
  d$temp_pred <- predict(m, newdata = d)
  d
})

# De-seasonalized
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
ggsave("nasa-colored-map.png", height=10, width=18)
       
resids <- glyphs(resids, "long", "day", "lat", "temp_resid") 
qplot(gx, gy, data = resids, geom = "line", group = gid) + map

# Plot predicted values
qplot(long, lat, data=month_preds, geom="tile", fill=pred, facets=year~month) + scale_fill_gradient2(midpoint=296.9, mid="white") + map

# Plot glyphs
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
qplot(gx, gy, data = month_preds, geom = "path", group = gid) + map 

year_preds <- glyphs(year_preds, "long", "year", "lat", "pred") 
year_preds <- glyphs(year_preds, "long", "year", "lat", "pred", height=3) 
qplot(gx, gy, data = year_preds, geom = "path", group = gid, xlab="", ylab="") + map + coord_map()
ggsave("nasa-deseas-trend.pdf", height=8, width=8)
       
year_resids <- glyphs(year_preds, "long", "year", "lat", "pred", height=3) 
qplot(gx, gy, data = year_preds, geom = "path", group = gid, xlab="", ylab="") + map + coord_map()
ggsave("nasa-deseas-trend.pdf", height=8, width=8)

# Explain icons
nasa.temp<-data.frame(time=nasa$time, temp=nasa$surftemp)
nasa.temp<-cast(nasa.temp, formula=temp~time)
nasa.oneloc <- subset(nasa, x == 22 & y == 21, select=c("id","time","surftemp"))
nasa.twoloc <- subset(nasa, x == 7 & y == 12, select=c("id","time","surftemp"))
nasa.threeloc <- subset(nasa, x == 12 & y == 3, select=c("id","time","surftemp"))
nasa.sub<-rbind(nasa.oneloc, nasa.twoloc, nasa.threeloc)
#nasa.oneloc<-t(as.matrix(nasa.oneloc$surftemp))
#colnames(nasa.oneloc)<-paste("t", 1:72, sep="")
nasa.oneloc$gx<-cos(2*pi*(nasa.oneloc$time/72)) * ((nasa.oneloc$surftemp-293)/(302-293))
nasa.oneloc$gy<-sin(2*pi*(nasa.oneloc$time/72)) * ((nasa.oneloc$surftemp-293)/(302-293))
qplot(time, surftemp, data=nasa.oneloc, geom="path", xlab="Time", ylab="Temperature (Kelvin)")
ggsave("nasa-oneloc-ts.pdf", height=2, width=8)
qplot(gx, gy, data=nasa.oneloc, geom="path") + theme_nothing()

nasa.sub$lon<-rep(c(2,4,6), rep(72,3))
nasa.sub$lat<-rep(1,216)

nasa.sub<-glyphs(nasa.sub, "lon", "time", "lat", "surftemp", width=0.5, height=3, polar=TRUE)

qplot(time, surftemp, data=nasa.sub, geom="path", xlab="Time", ylab="Temperature (Kelvin)", group=id, colour=gid)
ggsave("nasa-glyph-ts.pdf", height=2, width=8)

qplot(gx, gy, data=nasa.sub, geom="polygon", colour=gid, fill=I(NA)) + theme_nothing()
ggsave("nasa-glyph.pdf", height=2, width=6)
