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

outlines <- as.data.frame(map("state",
                              plot=FALSE)[c("x","y")])
map <- c(
  geom_path(aes(x, y), inherit.aes = FALSE, 
    data = outlines, colour = alpha("grey60", 0.4))
)

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

# Ok, this now works for transforming the lon, and selecting the USA
temp.usa<-temp[114:150, 20:33,] # lon -130, -65; lat 25, 50
temp.usa.melt<-melt(temp.usa)
colnames(temp.usa.melt)<-c("gridx", "gridy", "time", "temp")
temp.usa.melt$lon<-lon[temp.usa.melt$gridx+113]
temp.usa.melt$lat<-lat[temp.usa.melt$gridy+19]

temp.usa.melt.gly<-glyphs(temp.usa.melt, "lon", "time", "lat", "temp")
qplot(gx, gy, data=temp.usa.melt.gly, group=gid, geom="line") + map + coord_map()

library(plyr)
temp.mean<-ddply(temp.usa.melt, c("lon","lat"), summarise, tm=mean(temp, na.rm=T))
qplot(lon, lat, data=temp.mean, colour=tm) + map + coord_map()

