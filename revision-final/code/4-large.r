library(ncdf)
library(reshape2)
library(ggplot2)
library(lubridate)

# Predictions
source("maps.r")
source("glyph.r")

# Load data ------------------------------------------------------------------

if (file.exists("temp_usa.rds")) {
  temp_usa <- readRDS("temp_usa.rds")
} else {
  if (!file.exists("air.2x2.250.mon.anom.land.nc")) {
  download.file("ftp://ftp.cdc.noaa.gov/Datasets/gistemp/landonly/250km/air.2x2.250.mon.anom.land.nc", "air.2x2.250.mon.anom.land.nc")
  }

  fo<-open.ncdf("air.2x2.250.mon.anom.land.nc")
  fo$nvars
  lon <- get.var.ncdf(fo, "lon")
  lat <- get.var.ncdf(fo, "lat")
  time <- get.var.ncdf(fo, "time")
  temp <- get.var.ncdf(fo, "air")
  # Convert lon from 1 to 360 to -180 to 180
  lon <- (lon + 180) %% 360 - 180

  length(lon)
  dim(temp)

  # Ok, this now works for transforming the lon, and selecting the USA
  temp.usa <- temp[114:150, 20:33,] # lon -130, -65; lat 25, 50
  temp_usa <- melt(temp.usa)
  colnames(temp_usa) <- c("gridx", "gridy", "time", "temp")
  temp_usa$lon <- lon[temp_usa$gridx + 113]
  temp_usa$lat <- lat[temp_usa$gridy + 19]

  temp_usa <- subset(temp_usa, !(lon > -70 & lat < 37))

  library(lubridate)
  temp_usa$date <- ymd(18800101) + months(temp_usa$time - 1)
  temp_usa$year <- year(temp_usa$date)
  temp_usa$month <- month(temp_usa$date)  
  
  saveRDS(temp_usa, "temp_usa.rds")
}

temp_usa <- ddply(temp_usa, c("lon","lat"), function(df) {
 if (all(is.na(df$temp))) NULL else df 
})

# Plots of raw data -------------------------------------------------------

# temp_glyphs <- glyphs(temp_usa, "lon", "time", "lat", "temp")
# 
# ggplot(temp_glyphs, aes(gx, gy, group = gid)) + 
#   map_gistemp +
#   geom_line(aes(y = lat), colour = "white", size = 1.2) +
#   geom_path() +
#   add_ref_boxes(temp_glyphs) +
#   theme_fullframe()
# ggsave("../images/gistemp-raw.png", width = 8, height = 4)

temp_polar <- glyphs(temp_usa, "lon", "time", "lat", "temp", polar = T)

ggplot(temp_polar, aes(gx, gy, group = gid)) + 
  map_gistemp +
  geom_line(aes(y = lat), colour = "white", size = 1.2) +
  add_ref_boxes(temp_polar) +
  add_ref_lines(temp_polar) +
  geom_path() +
  theme_fullframe()
ggsave("../images/gistemp-polar-raw.png", width = 8, height = 4)

temp_leg <- subset(temp_usa, gridx %% 5 == 1 & gridy %% 5 == 1 & !is.na(temp))
ggplot(temp_leg, aes(date, temp)) + 
  geom_path(aes(group = interaction(lat, lon)), alpha = 1/5) + 
  annotate("line", range(temp_usa$date), 0, colour = "white") +  
  ylim(range(temp_usa$temp, na.rm = TRUE)) +
  scale_x_datetime("Date",
    breaks = ymd(18800101) + 20 * years(0:6), 
    labels = c("1880", "1900", "1920", "1940", "1960", "1980", "2000")) + 
  ylab("Temperature (C)") +
  coord_polar()
ggsave("../images/gistemp-polar-legend.pdf", width = 4, height = 4)


# Plots of predictions -------------------------------------------------------

# Just use 1950-2010, cleaner data
temp_usa.sub <- subset(temp_usa, year > 1950 & year < 2011 &
  !is.na(temp))

temp_models <- dlply(temp_usa.sub, c("lat", "lon"), function(df) {
  lm(temp ~ year + factor(month), data = df)
})

# month_grid <- expand.grid(year = 1950:2010, month = 1:12)
# month_preds <- ldply(temp_models, function(mod) {
#   month_grid$pred <- predict(mod, newdata = month_grid)
#   month_grid
# })

year_grid <- expand.grid(year = unique(temp_usa.sub$year), month = 1)
year_preds <- ldply(temp_models, function(mod) {
  year_grid$pred <- predict(mod, newdata = year_grid)
  year_grid
})

year_glyphs <- glyphs(year_preds, "lon", "year", "lat", "pred")
ggplot(year_glyphs, aes(gx, gy, group = gid)) + 
  map_gistemp + 
  geom_line(aes(y = lat), colour = "white", size = 1.5) +
  add_ref_lines(year_glyphs) + 
  geom_path() + 
  theme_fullframe()
ggsave("../images/gistemp-pred.pdf", width = 8, height = 4)

ggplot(year_preds, aes(year, pred)) + 
  geom_path(aes(group = interaction(lat, lon)), alpha = 1/10) + 
  xlab("Year") + 
  ylab("Predicted temperature (C)")
ggsave("../images/gistemp-pred-legend.pdf", width = 4, height = 4)

# temp_usa.sub.gly<-glyphs(year_preds, "lon", "year", "lat", "pred",
#  polar = T)
# ggplot(temp_usa.sub.gly, aes(gx, gy, group = gid)) + 
#   map + # Need to get the circles drawn here
#   add_ref_lines(temp_usa.sub.gly) + 
#   add_ref_boxes(temp_usa.sub.gly) + 
#   geom_path() +
#   theme_fullframe()
# ggsave("../images/gistemp-polar-pred.png", width = 8, height = 4)


# Testing/checking
# library(plyr)
# temp.mean<-ddply(temp_usa, c("lon","lat"), summarise, tm=mean(temp, na.rm=T))
# qplot(lon, lat, data=temp.mean, colour=tm) + map + coord_map()
# 
