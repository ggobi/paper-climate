source("glyph-utils.r")
source("maps.r")

library("ggplot2")

if (!file.exists("../data/nasadata.rds")) {
  library("lubridate")

  nasa <- read.csv("../data/nasadata.csv.bz2", stringsAsFactors = FALSE)
  nasa$date <- ymd(nasa$date)
  nasa$day <- (nasa$date - min(nasa$date)) %/% ddays(1)
  nasa$month <- month(nasa$date)
  nasa$year <- year(nasa$date)
  
  saveRDS(nasa, "../data/nasadata.rds")
} else {
  nasa <- readRDS("../data/nasadata.rds")
}

  
theme_fullframe <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), 
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), 
    axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), 
    axis.ticks.margin = unit(0, "lines"), 
    legend.position = "none", 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), 
    panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_blank(), 
    plot.margin = unit(rep(2, 4), "mm")
  ), class = "options")
}

ref_boxes <- list(
  geom_tile(aes(lon, lat), colour = "white", fill = NA, inherit.aes = FALSE),
  scale_x_continuous(breaks = NA, expand = c(0, 0)),
  scale_y_continuous(breaks = NA, expand = c(0, 0))
)


map_mini <- c(map, list(
  coord_cartesian(xlim = c(-114.93, -85), ylim = c(7.55, 36.64), wise = T)))

