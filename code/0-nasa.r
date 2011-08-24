library("maps")
library("ggplot2")

if (!file.exists("nasadata.rds")) {
  library("lubridate")

  nasa <- read.csv("nasadata.csv.bz2", stringsAsFactors = FALSE)
  nasa$date <- ymd(nasa$date)
  nasa$day <- (nasa$date - min(nasa$date)) %/% ddays(1)
  nasa$month <- month(nasa$date)
  nasa$year <- year(nasa$date)
  
  saveRDS(nasa, "nasadata.rds")
} else {
  nasa <- readRDS("nasadata.rds")
}

outlines <- as.data.frame(map("world", plot = FALSE,
  xlim = -c(113.8, 56.2), ylim = c(-21.2, 36.2))[c("x","y")])
map <- list(
  geom_path(aes(x, y), inherit.aes = FALSE, data = outlines, alpha = 0.4,
    legend = FALSE),
  scale_x_continuous(breaks = NA),
  scale_y_continuous(breaks = NA), 
  xlab(NULL),
  ylab(NULL),
  coord_cartesian(xlim = c(-114.93, -55.07), ylim = c(-21.53, 36.64), wise = T))


tl 
map_mini <- c(map, list(
  coord_cartesian(xlim = c(-114.93, -85), ylim = c(7.55, 36.64), wise = T)))

