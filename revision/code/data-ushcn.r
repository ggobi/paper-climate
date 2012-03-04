library(maps)
library(ggplot2)
library(plyr)

source("maps.r")

ushcn.loc <- "../../data/ushcndata.rds"
ushcn.stations.loc <- "../../data/ushcnstations.rds"

if (!file.exists(ushcn.loc)) {
  temp.all<-read.table("../data/9641C_201012_raw.avg")
  colnames(temp.all)<-c("year", "jan", "feb", "mar", "apr", "may", "jun", "jul",
    "aug", "sep", "oct", "nov", "dec", "ann")
  temp.all$stn<-temp.all$year%/%100000
  temp.all$year<-temp.all$year%%10000

  stn.all<-read.table("../../data/ushcn-stations.txt", fill=T)
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
