rescale01 <- function(x, xlim=NULL) {
  if (is.null(xlim)) {
	  rng <- range(x, na.rm = TRUE)
   } else {
   	  rng <- xlim
   }
   (x - rng[1]) / (rng[2] - rng[1])
}
rescale11 <- function(x, xlim=NULL) 2 * rescale01(x, xlim) - 1
  
glyphs <- function(data, x_major, x_minor, y_major, y_minor, polar = F, height = NULL, width = NULL, ylim=NULL) {
  data$gid <- interaction(data[[x_major]], data[[y_major]], drop = TRUE)
  
  if (is.null(width)) {
    width <- resolution(data[[x_major]]) * 0.95    
    message("Using width ", format(width, digits = 3))
  }
  else {
    width <- resolution(data[[x_major]]) * width    
    message("Using width ", format(width, digits = 3))
  }
    
  if (is.null(height)) {
    height <- resolution(data[[y_major]]) * 0.95    
    message("Using height ", format(height, digits = 3))
  }
  else {
    height <- resolution(data[[y_major]]) * height    
    message("Using height ", format(height, digits = 3))
  }
  
  if (polar) {
  	xlim <- range(data[[x_minor]]) + c(-0.5, 0.5)*resolution(data[[x_minor]])
    theta <- 2 * pi * rescale01(data[[x_minor]], xlim)
    r <- rescale01(data[[y_minor]], ylim)
    
    data$gx <- data[[x_major]] + width  / 2 * r * sin(theta) 
    data$gy <- data[[y_major]] + height / 2 * r * cos(theta)
    data <- data[order(data[[x_major]], data[[x_minor]]), ] 
  } else {
    data$gx <- data[[x_major]] + rescale11(data[[x_minor]]) * width / 2
    data$gy <- data[[y_major]] + rescale11(data[[y_minor]], ylim) * height / 2
  }
  
  data
}
