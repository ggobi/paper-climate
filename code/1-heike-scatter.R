library(ggplot2)
library(maps)

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
nasa <- getbox(world, xlim = c(-114.93, -55.07), ylim = c(-21.53, 36.64))


map <- list(
  geom_polygon(aes(long, lat, group=group), inherit.aes = FALSE, data = nasa, fill="grey80", 
    legend = FALSE),
  scale_x_continuous(breaks = NA),
  scale_y_continuous(breaks = NA), 
  xlab(NULL),
  ylab(NULL),
  coord_cartesian(xlim = c(-114.93, -55.07), ylim = c(-21.53, 36.64), wise = T))

ggplot() + map + geom_point(aes(gx, gy), data = temp, shape = ".")