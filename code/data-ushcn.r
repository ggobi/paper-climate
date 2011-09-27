library(maps)
library(ggplot2)
library(plyr)

source("glyph-utils.r")

states <- map_data("state")
world <- map_data("world")
states$group <- max(world$group) + states$group
both <- rbind(world, states)
both <- getbox(both, xlim = c(-126, -65), ylim = c(24, 50))

both <- ddply(both, "group", function(df) {
  if (diff(range(df$long)) < 1e-6) return(NULL)
  if (diff(range(df$lat)) < 1e-6) return(NULL)
  df
})

map_ushcn <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = subset(both, region != "Great Lakes"), legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(breaks = NA, expand = c(0.02, 0)),
  scale_y_continuous(breaks = NA, expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

ushcn.loc <- "../data/ushcndata.rds"
ushcn.stations.loc <- "../data/ushcnstations.rds"

if (!file.exists(ushcn.loc)) {
  temp.all<-read.table("../data/9641C_201012_raw.avg")
  colnames(temp.all)<-c("year", "jan", "feb", "mar", "apr", "may", "jun", "jul",
    "aug", "sep", "oct", "nov", "dec", "ann")
  temp.all$stn<-temp.all$year%/%100000
  temp.all$year<-temp.all$year%%10000

  stn.all<-read.table("../data/ushcn-stations.txt", fill=T)
  stn.all<-stn.all[,c(1:3,5:6)]
  colnames(stn.all)<-c("stn", "lat", "lon", "state", "town")
  stn.all$name<-paste(stn.all$town, stn.all$state)
  ushcn.stations <- stn.all
  
  ushcn.all <- join(temp.all, stn.all)
  ushcn <- melt(ushcn.all[ , -c(14, 19)], 
    id=c("stn", "year", "lat", "lon", "name", "state"))
  ushcn <- rename(ushcn, 
      c("variable" = "month", "value" = "temp"))

  ushcn <- mutate(ushcn, 
      temp = temp/10,
      date = as.Date(paste(year, "-", as.numeric(month), "-01", sep = "")),
      )    
  ushcn$temp[ushcn$temp == -999.9] = NA
  
  saveRDS(ushcn, ushcn.loc)
  saveRDS(ushcn.stations, ushcn.stations.loc)
} else {
  ushcn <- readRDS(ushcn.loc)
  ushcn.stations <- readRDS(ushcn.stations.loc)
}
