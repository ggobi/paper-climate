library(maps)

# All
temp.all<-read.table("9641C_201012_raw.avg")
colnames(temp.all)<-c("year", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec", "ann")
temp.all$stn<-temp.all$year%/%100000
temp.all$year<-temp.all$year%%10000

temp.01.10 <- subset(temp.all, year > 2000)
  
stn.all<-read.table("ushcn-stations.txt", fill=T)
stn.all<-stn.all[,c(1:3,5:6)]
colnames(stn.all)<-c("stn", "lat", "lon", "state", "town")
stn.all$name<-paste(stn.all$town, stn.all$state)

temp.01.10<-merge(temp.01.10, stn.all, by="stn")

temp.01.10.melt<-melt(temp.01.10[,-c(15, 19)], id=c("stn", "year", "lat", "lon", "name", "state"))
colnames(temp.01.10.melt)[7]<-"month"
colnames(temp.01.10.melt)[8]<-"temp"
temp.01.10.melt$temp<-temp.01.10.melt$temp/10
temp.01.10.melt$date<-as.Date(paste(temp.01.10.melt$year, "-", as.numeric(temp.01.10.melt$month), "-01", sep=""))
temp.01.10.melt$temp[temp.01.10.melt$temp==(-999.9)]<-NA
temp.01.10.melt$time<-(temp.01.10.melt$year-2001)*12 + as.numeric(temp.01.10.melt$month)

iowa<-subset(temp.01.10.melt, state == "IA")
qplot(lon, lat, data=iowa)
iowa.gly <- glyphs(iowa, "lon", "time", "lat", "temp", width=50, height=50) 
qplot(gx, gy, data=iowa.gly, group=gid, geom="line")

colorado<-subset(temp.01.10.melt, state == "CO")
qplot(lon, lat, data=colorado)
colorado.gly <- glyphs(colorado, "lon", "time", "lat", "temp", width=100, height=100) 
qplot(gx, gy, data=colorado.gly, group=gid, geom="line")


models <- dlply(temp.01.10.melt, .(stn), function(df) {
  df$mpi <- mpi(df$time)
  lm(temp ~ time + sin(mpi) + cos(mpi), data=df)
})

preds <- ldply(models, function(m) {
  coefs <- coef(m)
  data.frame(
    time = c(1, 1392),
    pred = coefs[1] + coefs[2] * c(1, 1392),
    slope = rep(coefs[2], 2),
    deviance = deviance(m),
    rsq = summary(m)$r.squared             
  )
})

stn.all.mod <- merge(stn.all, preds)

resids <- ldply(models, function(m) {
  coefs <- coef(m)
  data.frame(
    resid = resid(m), 
    time = m$model$time,
    pred = coefs[1] + coefs[2] * m$model$time
  )
})

stn.all.mod <- transform(stn.all.mod, 
  rtime = rescaler(time, type="range") - 0.5,
  rpred = rescaler(pred, type="range") - 0.5
)
stn.all.mod <- transform(stn.all.mod, 
  x = lon + rtime,
  ypred = lat + rpred
)

outlines <- as.data.frame(map("state",
                              plot=FALSE)[c("x","y")])
map <- c(
  geom_path(aes(x, y), inherit.aes = FALSE, 
    data = outlines, colour = alpha("grey60", 0.4))
)

qplot(x, ypred, data=stn.all.mod, geom="line", group=stn, xlim=c(-125, -68), ylim=c(23, 50), xlab="", ylab="", colour=slope, size=I(2), alpha=I(0.7)) + map 
qplot(slope, data=stn.all.mod, geom="histogram")

write.csv(stn.all.mod, "US-temperature.csv", row.names=F, quote=F)
