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

	return(small)
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

add_ref_lines <- function(data, colour = "white", size = 1.5, ...){
  rl <- ref_lines(data)
  list(geom_line(data = rl, colour = colour, size = size, ...))
}

add_ref_boxes <- function(data, colour = "white", size = 0.5, fill = NA, ...){
  rb <- ref_boxes(data)
  list(geom_rect(aes_all(names(rb)), data = rb,
     colour = colour, size = size, fill = fill, inherit.aes = FALSE, ...))
}

# Need to have a function for the circles references for polar coords
# Need to have a function for the reference lines
# Need to have reference boxes for the irregular grid, too
