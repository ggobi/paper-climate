amp <- 30
mu <- 330

t <- seq(0, pi, length=12)
x1 <- mu+amp*sin(t)
x2 <- mu+30-amp*sin(t)
x3 <- rep(mu+15, length(t))

locs <- data.frame(cbind(rep(t,3), c(x1,x2,x3), rep(1:3, each=12), rep(1,3*12)))
names(locs) <- c("time","temp","x_max", "y_max")
qplot(time,temp, data=locs, colour=x_max)

locs2 <- data.frame(cbind(rep(t+pi,3), rep(c(mean(x1),mean(x2),mean(x3)), each=12), rep(1:3, each=12), rep(1,3*12)))
names(locs2) <- c("time","temp","x_max", "y_max")


# multiple lines
locs <- glyphs(locs, "x_max", "time", "y_max", "temp") 
locs2 <- glyphs(locs2, "x_max", "time", "y_max", "temp", ylim=range(locs$temp)) 
ggplot(locs, aes(gx, gy, group = gid)) + 
  geom_path() +
  geom_path(aes(gx, gy, group = gid), data=locs2, colour="red") +
  geom_tile(aes(x_max, y_max), colour = "white", fill = NA)



# now in polar coordinates:
locs <- glyphs(locs, "x_max", "time", "y_max", "temp", polar = T) 
locs2 <- glyphs(locs2, "x_max", "time", "y_max", "temp", ylim=range(locs$temp), polar = T) 
ggplot(locs, aes(gx, gy, group = gid)) + 
  geom_path() +
  geom_path(aes(gx, gy, group = gid), data=locs2, colour="red") +
  geom_tile(aes(x_max, y_max), colour = "white", fill = NA)

