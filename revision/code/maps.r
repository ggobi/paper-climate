library("maps")
library("ggplot2")

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

	# Remove slivvers
	small <- ddply(small, "group", function(df) {
	  if (diff(range(df$long)) < 1e-6) return(NULL)
	  if (diff(range(df$lat)) < 1e-6) return(NULL)
	  df
	})

	small
}

##### Prepping Data
world <- map_data("world")

states <- map_data("state")
states$group <- max(world$group) + states$group
both <- rbind(world, states)

##### Maps
.nasa <- getbox(both, xlim = c(-115, -55), ylim = c(-21.1, 36.6))
map_nasa <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = .nasa, legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

ushcn <- subset(getbox(both, xlim = c(-126, -65), ylim = c(24, 52)),
  region != "Great Lakes")
map_ushcn <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = ushcn, legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

gistemp <- subset(getbox(both, xlim = c(-135, -57), ylim = c(22, 53)),
  region != "Great Lakes")
map_gistemp <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = gistemp, legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))



