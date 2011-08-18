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
map <- geom_path(aes(x, y), inherit.aes = FALSE, data = outlines, 
  alpha = 0.4, legend = FALSE)

