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

getbox <- function (map, xlim, ylim) {
	# identify all regions involved
	small <- subset(map, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]))
	regions <- unique(small$region)
	small <- subset(map, region %in% regions)	
	
	# now shrink all nodes back to the bounding box
	small$long <- pmax(small$long, xlim[1])
	small$long <- pmin(small$long, xlim[2])
	small$lat <- pmax(small$lat, ylim[1])
	small$lat <- pmin(small$lat, ylim[2])

	return(small)
}

world <- map_data("world")
world <- getbox(world, xlim = c(-114.93, -55.07), ylim = c(-21.53, 36.64))
# Remove slivvers

world <- ddply(world, "group", function(df) {
  if (diff(range(df$long)) < 1e-6) return(NULL)
  if (diff(range(df$lat)) < 1e-6) return(NULL)
  df
})

map <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = world, legend = FALSE, fill = "grey80", colour = "white"),
  scale_x_continuous(breaks = NA, expand = c(0.02, 0)),
  scale_y_continuous(breaks = NA, expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))


map_mini <- c(map, list(
  coord_cartesian(xlim = c(-114.93, -85), ylim = c(7.55, 36.64), wise = T)))

