library(ncdf)
fo<-open.ncdf("air.mon.anom.nc")
fo$nvars
lon <- get.var.ncdf(fo, "lon")
lat <- get.var.ncdf(fo, "lat")
time <- get.var.ncdf(fo, "time")
temp <- get.var.ncdf(fo, "air")

temp.usa<-temp[10:24, 8:14,] # lon -130, 65; lat 25, 50
temp.usa.melt<-melt(temp.usa)
colnames(temp.usa.melt)<-c("gridx", "gridy", "time", "temp")
temp.usa.melt$lon<-lon[temp.usa.melt$gridx+9]
temp.usa.melt$lat<-lat[temp.usa.melt$gridy+7]

temp.usa.melt.gly<-glyphs(temp.usa.melt, "lon", "time", "lat", "temp")
qplot(gx, gy, data=temp.usa.melt.gly, group=gid, geom="line")

library(maps)
world <- map_data("world")
world <- getbox(world, xlim = c(-138, -55), ylim = c(22, 52))
world <- ddply(world, "group", function(df) {
  if (diff(range(df$long)) < 1e-6) return(NULL)
  if (diff(range(df$lat)) < 1e-6) return(NULL)
  df
})

colnames(world)[1]<-"lon"

map <- list(
  geom_polygon(aes(lon, lat, group = group), inherit.aes = FALSE, 
    data = world, legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(breaks = NA, expand = c(0.02, 0)),
  scale_y_continuous(breaks = NA, expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

qplot(gx, gy, data=temp.usa.melt.gly, group=gid, geom="line") + map

fo<-open.ncdf("air.2x2.250.mon.anom.land.nc")
fo$nvars
lon <- get.var.ncdf(fo, "lon")
lat <- get.var.ncdf(fo, "lat")
time <- get.var.ncdf(fo, "time")
temp <- get.var.ncdf(fo, "air")
# Convert lon from 1 to 360 to -180 to 180
lon<-(lon+180)%%360 - 180

length(lon)
dim(temp)

library(ggplot2)
# Ok, this now works for transforming the lon, and selecting the USA
temp.usa<-temp[114:150, 20:33,] # lon -130, -65; lat 25, 50
temp.usa.melt<-melt(temp.usa)
colnames(temp.usa.melt)<-c("gridx", "gridy", "time", "temp")
temp.usa.melt$lon<-lon[temp.usa.melt$gridx+113]
temp.usa.melt$lat<-lat[temp.usa.melt$gridy+19]

temp.usa.melt.gly<-glyphs(temp.usa.melt, "lon", "time", "lat", "temp")
qplot(gx, gy, data=temp.usa.melt.gly, group=gid, geom="line") + map + coord_map() # Testing
p <-ggplot(temp.usa.melt.gly, aes(gx, gy, group = gid)) + 
  map
p <- p + geom_line(aes(y = lat), colour = "grey90", size = 1.5) 
p <- p + geom_path() 
p <- p + ref_boxes # This part is really slow
p <- p + theme_fullframe() + coord_map() # but not as slow as this part
ggsave("../images/gistemp-raw.png", width = 8, height = 4)

temp.usa.melt.gly<-glyphs(temp.usa.melt, "lon", "time", "lat", "temp", polar=T)
ggplot(temp.usa.melt.gly, aes(gx, gy, group = gid)) + 
  map + 
  geom_path() +
  ref_boxes +
  theme_fullframe() + coord_map()
ggsave("../images/gistemp-polar-raw.png", width = 8, height = 4)

# Predictions
library(lubridate)
temp.usa.melt$date <- ymd(18800101) + months(temp.usa.melt$time-1)
temp.usa.melt$year <- year(temp.usa.melt$date)
temp.usa.melt$month <- month(temp.usa.melt$date)
# Just use 1950-2010, cleaner data
temp.usa.melt.sub<-subset(temp.usa.melt, year>1950 & year<2011)
temp.usa.melt.sub<-subset(temp.usa.melt.sub, !is.na(temp))
#temp_models %<-cache% dlply(temp.usa.melt.sub, c("lat", "lon"), function(df) {
#  lm(temp ~ year + factor(month), data = df)
#}) This uses old data!!!
temp_models <- dlply(temp.usa.melt.sub, c("lat", "lon"), function(df) {
  lm(temp ~ year + factor(month), data = df)
})
month_grid <- expand.grid(year = 1950:2010, month = 1:12)
month_preds <- ldply(temp_models, function(mod) {
  month_grid$pred <- predict(mod, newdata = month_grid)
  month_grid
})
year_grid <- expand.grid(year = unique(temp.usa.melt.sub$year), month = 1)
year_preds <- ldply(temp_models, function(mod) {
  year_grid$pred <- predict(mod, newdata = year_grid)
  year_grid
})
temp.usa.melt.sub.gly<-glyphs(year_preds, "lon", "year", "lat", "pred")
ggplot(temp.usa.melt.sub.gly, aes(gx, gy, group = gid)) + 
  map + geom_line(aes(y = lat), colour = "grey80", size = 1.5) +
  geom_path() +
  ref_boxes +
  theme_fullframe() + coord_map()
ggsave("../images/gistemp-pred.png", width = 8, height = 4)
# Coord map seems to make the plotting REALLY SLOW!

temp.usa.melt.sub.gly<-glyphs(year_preds, "lon", "year", "lat", "pred", polar=T)
ggplot(temp.usa.melt.sub.gly, aes(gx, gy, group = gid)) + 
  map + # Need to get the circles drawn here
  geom_path() +
  ref_boxes +
  theme_fullframe() + coord_map()
ggsave("../images/gistemp-polar-pred.png", width = 8, height = 4)


# Testing/checking
library(plyr)
temp.mean<-ddply(temp.usa.melt, c("lon","lat"), summarise, tm=mean(temp, na.rm=T))
qplot(lon, lat, data=temp.mean, colour=tm) + map + coord_map()

