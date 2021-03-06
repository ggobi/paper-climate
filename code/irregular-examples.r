library(mgcv)

source("glyph.r")
source("glyph-utils.r")
source("overlapping.r")
source("data-ushcn.r")

temp.post1950 <- subset(ushcn, year >= 1950)

season_index <- function(time){
  (time %% 12  + 1 )/ 12
}

temp.post1950 <- mutate(temp.post1950, 
    time = (year - 1950) * 12 + as.numeric(month),
    season = season_index(time)
    )    

width.mod <- (diff(range(ushcn$lon))/diff(range(gistemp$lon)))/(diff(range(ushcn$lat))/diff(range(gistemp$lat)))

#==== Modelling

# seasonal.fit.post1950 <- dlply(temp.post1950, "stn", function(df){
#   gam(temp ~ s(season, bs = "cc"), 
#     knots = list(season = c(0, 12)),
#     data = df, na.action = "na.exclude")
#   }, .progress = "text"
# ) 
fit.post1950 <- dlply(temp.post1950, "stn", function(df){
  lm(temp ~ year + month, 
    data = df, na.action = "na.exclude")
  }, .progress = "text"
) 

temp.post1950 <- cbind(temp.post1950, 
  ldply(fit.post1950, function(fit) {
    data.frame(res = residuals(fit), pred = predict(fit))},
  .progress = "text"))

lin.pred.post1950 <- ldply(fit.post1950, function(fit){
    one.df <- data.frame(year = 1950:2010, 
      month = "jan")
    one.df$pred <- predict(fit, one.df)
    one.df
  }, .progress = "text")

lin.pred.post1950 <- merge(lin.pred.post1950, ushcn.stations)

w <- 1
h <- 1

lin.pred.gly <- glyphs(lin.pred.post1950, "lon", "year", "lat", "pred", 
  width=w, height=h, y_scale = mean0)

ggplot(lin.pred.gly, aes(gx, gy, group = gid)) + 
  map_ushcn + 
  add_ref_lines(lin.pred.gly) +
  geom_path() +
  theme_fullframe() 
ggsave("../images/usa-lin-overlap.png", width = 8, height = 4.5)

# Mountain states: Colorado, Wyoming, Idaho, Montana, Nevada, Utah
mountain.states <- c("CO", "WY", "ID", "MT", "NV", "UT")
mountains.gly<-glyphs(subset(lin.pred.post1950, state %in% mountain.states),
  "lon", "year", "lat", "pred", width=.5, height=.5, y_scale = mean0)

ggplot(mountains.gly, aes(gx, gy, group = gid)) + 
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = both, legend = FALSE, fill = "grey80", colour = "grey90") +
  scale_x_continuous(breaks = NA, expand = c(0.02, 0)) +
  scale_y_continuous(breaks = NA, expand = c(0.02, 0)) +
  xlab(NULL) +
  ylab(NULL) +
  add_ref_lines(mountains.gly) +
  geom_path() +
  theme_fullframe() +
  coord_cartesian(xlim = c(-121, -101), ylim = c(34, 50)) 
ggsave("../images/ghcn-mountains.png", width = 4, height = 4.5)

#== one solution combine overlapping glyphs
stn.dists <- get.stn.dists(ushcn.stations, "lon", "lat")
grps <- combine.overlapping(stn.dists, "lon.dist", "lat.dist", w, h)

lin.pred.post1950.g <- merge(lin.pred.post1950[, c("stn", "year", "pred")],   
  grps, all = TRUE)

lin.pred.post1950.g$stn.id <- lin.pred.post1950.g$stn
lin.pred.post1950.g$stn <- lin.pred.post1950.g$group
 
lin.pred.post1950.g <- merge(lin.pred.post1950.g, ushcn.stations)

# scale by hand here becasue want to scale on stn.id level not grid cell level
lin.pred.post1950.g  <- ddply(lin.pred.post1950.g, "stn.id", mutate,
  pred.ind = pred - pred[year == min(year)])

lin.pred.collapsed <- glyphs(lin.pred.post1950.g, "lon", "year", "lat",
  "pred.ind",  width = w,  height = h)

# doesn't really work...might be better just to average within grid cells
ggplot(lin.pred.collapsed, aes(gx, gy, group = gid)) +
  map_ushcn + 
  add_ref_lines(lin.pred.collapsed) +
  add_ref_boxes(lin.pred.collapsed) + 
  geom_path(aes(group = stn.id)) +
  theme_fullframe() 

lin.pred.post1950.sum  <- ddply(lin.pred.post1950.g, c("year", "stn"),
  summarise, pred = mean(pred, na.rm = TRUE), n = length(pred), 
  lon = lon[1], lat = lat[1])

lin.pred.sum <- glyphs(lin.pred.post1950.sum, "lon", "year", "lat",
  "pred",  width = w,  height = h, y_scale = mean0)

# averaging within grid cells
ggplot(lin.pred.sum, aes(gx, gy, group = gid)) +
  map_ushcn + 
  add_ref_lines(lin.pred.sum) +
  geom_path(aes(colour = n > 1)) +
  theme_fullframe() +
  scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "grey40"))
ggsave("../images/usa-lin-collapse.png", width = 8, height = 4.5)



#== gridding
lin.pred.grid <- grid.all(lin.pred.post1950, "lon", "lat", w, h)

lin.pred.grid.sum  <- ddply(lin.pred.grid, c("year", "grid.x", "grid.y"),
  summarise, pred = mean(pred, na.rm = TRUE), n = length(pred), 
  grid.x = grid.x[1], grid.y = grid.y[1])

lin.pred.grid.gly <- glyphs(lin.pred.grid.sum, "grid.x", "year", "grid.y",
  "pred",  width = w,  height = h, y_scale = mean0)

ggplot(lin.pred.grid.gly,aes(gx, gy, group = gid)) +
  map_ushcn + 
  add_ref_lines(lin.pred.grid.gly) +
  geom_path(aes(colour = n > 1)) +
  theme_fullframe() +
  scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "grey40"))
ggsave("../images/usa-lin-grid.png", width = 8, height = 4.5)

#=== SEASONAL ===#

seas.pred.post1950 <- ldply(fit.post1950, function(fit){
    one.df <- data.frame(month = sort(unique(fit$model$month)), year = 1980)
    one.df$pred <- predict(fit, one.df)
    one.df
  }, .progress = "text")

seas.pred.post1950 <- merge(seas.pred.post1950, ushcn.stations)
seas.pred.post1950$time <- as.numeric(seas.pred.post1950$month)
seas.pred.post1950 <- ddply(seas.pred.post1950, "stn", mutate,
  avg = mean(pred, na.rm = TRUE))

seas.pred.gly <- glyphs(seas.pred.post1950, "lon", "time", "lat", "pred", 
  width=1, height=1, y_scale = mean0)

ggplot(seas.pred.gly, aes(gx, gy, group = gid)) + 
  map_ushcn + 
  add_ref_boxes(seas.pred.gly, "avg", colour = NA, alpha = 0.2) +
  geom_path() +
  theme_fullframe()

##=== collapsing
seas.pred.post1950.g <- merge(seas.pred.post1950[, c("stn", "time", "pred")],   
  grps, all = TRUE)
seas.pred.post1950.g$stn.id <- seas.pred.post1950.g$stn
seas.pred.post1950.g$stn <- seas.pred.post1950.g$group
 
seas.pred.post1950.g <- merge(seas.pred.post1950.g, ushcn.stations)
seas.pred.post1950.g <- ddply(seas.pred.post1950.g, "stn", mutate,
  avg = mean(pred, na.rm = TRUE))

seas.pred.collapsed <- glyphs(seas.pred.post1950.g, "lon", "time", "lat",
  "pred",  width = w,  height = h, y_scale = mean0)

ggplot(seas.pred.collapsed, aes(gx, gy, group = gid)) +
  map_ushcn + 
  add_ref_boxes(seas.pred.collapsed, "avg", alpha = 0.2, colour = NA) +
  geom_line(aes(group = stn.id)) +
  theme_fullframe()  
ggsave("../images/usa-season-collapsed.png", width = 8, height = 4.5)

#== gridding
seas.pred.grid <- grid.all(seas.pred.post1950, "lon", "lat", w, h)
seas.pred.grid <- ddply(seas.pred.grid, c("grid.x", "grid.y"), mutate,
  avg = mean(unique(avg)))

glyph.seasonal.grid <- glyphs(seas.pred.grid, "grid.x", "time", "grid.y",   
  "pred", width = 0.95 * w, height = 0.95 * h, y_scale = mean0)

ggplot(glyph.seasonal.grid) +
  map_ushcn + 
  add_ref_boxes(glyph.seasonal.grid, "avg", alpha = 0.2, colour = NA) +
  geom_line(aes(gx, gy, group = stn)) +
  theme_fullframe() 
ggsave("../images/usa-season-grid.png", width = 8, height = 4.5)
