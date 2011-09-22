source("glyph.r")
source("glyph-utils.r")
# Template time series
amp <- 30
mu <- 300

t <- seq(0, pi, length=36)
x1 <- mu + seq(0, 30, length=36) # linear increase
x2 <- mu + seq(30, 0, length=36) # linear decrease
x3 <- mu + 30 + seq(0, 30, length=36) # linear increase + shift
x4 <- mu + 30 + seq(30, 0, length=36) # linear decrease + shift
x5 <- mu + amp*sin(t) # inverted U
x6 <- mu + 30-amp*sin(t) # shifted, inverted U
x7 <- mu + seq(0, 30, length=36) + amp*sin(t) # inverted U + linear
x8 <- mu + seq(0, 30, length=36) + 30-amp*sin(t) # shifted, inverted U + linear
x9 <- mu + amp*sin(t*6) # seasonal
x10 <- mu + amp*sin(t*6)/2 # seasonal + smaller amplitude
x11 <- mu + seq(0, 30, length=36) + amp*sin(t*6) # seasonal + linear
x12 <- mu + seq(0, 30, length=36) + amp*sin(t*6)/2 # seasonal + smaller amplitude + linear

locs <- data.frame(cbind(rep(t,12), c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12), c(rep(1:6, each=36), rep(1:6, each=36)), rep(2:1,each=36*6)))
names(locs) <- c("time","temp","x_max", "y_max")

locs2 <- data.frame(cbind(rep(t+pi,12), rep(c(mean(x1),mean(x2),mean(x3),mean(x4),mean(x5),mean(x6),mean(x7),mean(x8),mean(x9),mean(x10),mean(x11),mean(x12)), each=36), c(rep(1:6, each=36), rep(1:6, each=36)), rep(2:1,each=36*6)))
names(locs2) <- c("time","temp","x_max", "y_max")

library(ggplot2)
qplot(time,temp, data=subset(locs, x_max==1), geom="line")

# multiple lines
locs <- glyphs(locs, "x_max", "time", "y_max", "temp")
ggplot(locs, aes(gx, gy, group = gid)) + 
  geom_path(colour="grey80", data = ref_lines(locs)) +
  geom_path(aes(gx, gy, group = gid), colour="black") +
  geom_tile(aes(x_max, y_max), colour = "white", fill = NA) +
  theme_fullframe() + coord_equal()
ggsave("../images/euclid-to-polar-1.pdf")


# now in polar coordinates:
locs <- glyphs(locs, "x_max", "time", "y_max", "temp", polar = T) 
ggplot(locs, aes(gx, gy, group = gid)) + 
  geom_path(data = ref_lines(locs), colour="grey80") +
  geom_path() +
  geom_tile(aes(x_max, y_max), colour = "white", fill = NA) +
  theme_fullframe() + coord_equal()
ggsave("../images/euclid-to-polar-2.pdf")




# overall average temp as reference circle
locs2 <- data.frame(cbind(rep(t+pi,3), rep(c(mean(locs$temp)), each=3*12), rep(1:3, each=12), rep(1,3*12)))
names(locs2) <- c("time","temp","x_max", "y_max")
ggplot(locs, aes(gx, gy, group = gid)) + 
  geom_path(data = ref_lines(locs), colour="grey80") +
  geom_path() +
  geom_tile(aes(x_max, y_max), colour = "white", fill = NA)
