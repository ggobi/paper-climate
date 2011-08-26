library(ggplot2)
library(maps)
source("0-nasa.r")

ggplot() + map + geom_point(aes(gx, gy), data = temp, shape = ".")