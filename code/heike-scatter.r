library(ggplot2)
library(maps)
source("nasa.r")

ggplot() + map + geom_point(aes(gx, gy), data = temp, shape = ".")