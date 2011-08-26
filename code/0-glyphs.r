rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale11 <- function(x) 2 * rescale01(x) - 1
  
glyphs <- function(data, x_major, x_minor, y_major, y_minor, polar = F, height = NULL, width = NULL) {
  data$gid <- interaction(data[[x_major]], data[[y_major]], drop = TRUE)
  
  if (is.null(width)) {
    width <- resolution(data[[x_major]]) * 0.9    
    message("Using width ", format(width, digits = 3))
  }
  else {
    width <- resolution(data[[x_major]]) * width    
    message("Using width ", format(width, digits = 3))
  }
    
  if (is.null(height)) {
    height <- resolution(data[[y_major]]) * 0.9    
    message("Using height ", format(height, digits = 3))
  }
  else {
    height <- resolution(data[[y_major]]) * height    
    message("Using height ", format(height, digits = 3))
  }
  
  if (polar) {
    data$gx <- data[[x_major]] + width / 2 * rescale01(data[[y_minor]]) * 
      sin(2 * pi * rescale01(data[[x_minor]]))
    data$gy <- data[[y_major]] + height / 2 * rescale01(data[[y_minor]]) * 
      cos(2 * pi * rescale01(data[[x_minor]]))
    data <- data[order(data[[x_major]], data[[x_minor]]), ]
  } else {
    data$gx <- data[[x_major]] + rescale11(data[[x_minor]]) * width / 2
    data$gy <- data[[y_major]] + rescale11(data[[y_minor]]) * height / 2
  }
  
  data
}
