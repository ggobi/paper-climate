library(maps)
library(mgcv)
library(ggplot2)
library(plyr)

source("glyph.r")
source("glyph-utils.r")
source("overlapping.r")

states <- map_data("state")
world <- map_data("world")
states$group <- max(world$group) + states$group
both <- rbind(world, states)
both <- getbox(both, xlim = c(-126, -55.07), ylim = c(24, 50))

both <- ddply(both, "group", function(df) {
  if (diff(range(df$long)) < 1e-6) return(NULL)
  if (diff(range(df$lat)) < 1e-6) return(NULL)
  df
})

us.map <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
    data = both, legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(breaks = NA, expand = c(0.02, 0)),
  scale_y_continuous(breaks = NA, expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))


# All
temp.all<-read.table("../data/9641C_201012_raw.avg")
colnames(temp.all)<-c("year", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec", "ann")
temp.all$stn<-temp.all$year%/%100000
temp.all$year<-temp.all$year%%10000

stn.all<-read.table("../data/ushcn-stations.txt", fill=T)
stn.all<-stn.all[,c(1:3,5:6)]
colnames(stn.all)<-c("stn", "lat", "lon", "state", "town")
stn.all$name<-paste(stn.all$town, stn.all$state)

# Post 1950
temp.post1950 <- subset(temp.all, year >= 1950)
temp.post1950 <- merge(temp.post1950, stn.all, by="stn")

temp.post1950.melt <- melt(temp.post1950[,-c(15, 19)], 
  id=c("stn", "year", "lat", "lon", "name", "state"))
temp.post1950.melt <- rename(temp.post1950.melt, 
    c("variable" = "month", "value" = "temp"))

season_index <- function(time){
  (time %% 12  + 1 )/ 12
}

temp.post1950.melt <- mutate(temp.post1950.melt, 
    temp = temp/10,
    date = as.Date(paste(year, "-", as.numeric(month), "-01", sep = "")),
    time = (year - 1950) * 12 + as.numeric(month),
    season = season_index(time)
    )    
temp.post1950.melt$temp[temp.post1950.melt$temp == -999.9] = NA

#==== Modelling

# seasonal.fit.post1950 <- dlply(temp.post1950.melt, "stn", function(df){
#   gam(temp ~ s(season, bs = "cc"), 
#     knots = list(season = c(0, 12)),
#     data = df, na.action = "na.exclude")
#   }, .progress = "text"
# ) 
fit.post1950 <- dlply(temp.post1950.melt, "stn", function(df){
  lm(temp ~ year + month, 
    data = df, na.action = "na.exclude")
  }, .progress = "text"
) 

temp.post1950.melt <- cbind(temp.post1950.melt, 
  ldply(fit.post1950, function(fit) {
    data.frame(res = residuals(fit), pred = predict(fit))},
  .progress = "text"))

lin.pred.post1950 <- ldply(fit.post1950, function(fit){
    one.df <- data.frame(year = 1950:2010, 
      month = "jan")
    one.df$pred <- predict(fit, one.df)
    one.df
  }, .progress = "text")

lin.pred.post1950 <- merge(lin.pred.post1950, stn.all)

w <- 1
h <- 1

lin.pred.gly <- glyphs(lin.pred.post1950, "lon", "year", "lat", "pred", 
  width=w, height=h, y_scale = mean0)

ggplot(lin.pred.gly, aes(gx, gy, group = gid)) + 
  us.map + 
  add_ref_lines(lin.pred.gly) +
  geom_path() +
  theme_fullframe() 
ggsave("../images/usa-lin-overlap.png", width = 5.5, height = 4)

#== one solution combine overlapping glyphs
stn.dists <- get.stn.dists(stn.all, "lon", "lat")
grps <- combine.overlapping(stn.dists, "lon.dist", "lat.dist", w, h)

lin.pred.post1950.g <- merge(lin.pred.post1950[, c("stn", "year", "pred")],   
  grps, all = TRUE)

lin.pred.post1950.g$stn.id <- lin.pred.post1950.g$stn
lin.pred.post1950.g$stn <- lin.pred.post1950.g$group
 
lin.pred.post1950.g <- merge(lin.pred.post1950.g, stn.all)

# scale by hand here becasue want to scale on stn.id level not grid cell level
lin.pred.post1950.g  <- ddply(lin.pred.post1950.g, "stn.id", mutate,
  pred.ind = pred - pred[year == min(year)])

lin.pred.collapsed <- glyphs(lin.pred.post1950.g, "lon", "year", "lat",
  "pred.ind",  width = w,  height = h)

# doesn't really work...might be better just to average within grid cells
ggplot(lin.pred.collapsed, aes(gx, gy, group = gid)) +
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
  add_ref_lines(lin.pred.sum) +
  geom_path(aes(size = n > 1)) +
  theme_fullframe() +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = 0.25))
ggsave("../images/usa-lin-collapse.png", width = 4, height = 3)



#== gridding
lin.pred.grid <- grid.all(lin.pred.post1950, "lon", "lat", w, h)
glyph.lin.grid <- glyphs(lin.pred.grid, "grid.x", "year", "grid.y",   
  "pred", width = 0.95 * w, height = 0.95 * h, y_scale = mean0)

ggplot(glyph.lin.grid) +
  add_ref_lines(glyph.lin.grid) +
  geom_line(aes(gx, gy, group = stn)) +
  theme_fullframe() 
ggsave("../images/usa-lin-grid.pdf", width = 8, height = 6)


#=== SEASONAL ===#

seas.pred.post1950 <- ldply(fit.post1950, function(fit){
    one.df <- data.frame(month = sort(unique(fit$model$month)), year = 1980)
    one.df$pred <- predict(fit, one.df)
    one.df
  }, .progress = "text")

seas.pred.post1950 <- merge(seas.pred.post1950, stn.all)
seas.pred.post1950$time <- as.numeric(seas.pred.post1950$month)
seas.pred.post1950 <- ddply(seas.pred.post1950, "stn", mutate,
  avg = mean(pred, na.rm = TRUE))

seas.pred.gly <- glyphs(seas.pred.post1950, "lon", "time", "lat", "pred", 
  width=1, height=1, y_scale = mean0)

ggplot(seas.pred.gly, aes(gx, gy, group = gid)) + 
  add_ref_boxes(seas.pred.gly, "avg", colour = NA, alpha = 0.2) +
  geom_path() +
  theme_fullframe()
ggsave("../images/usa-season-overlap.pdf", width = 8, height = 6)

##=== collapsing
seas.pred.post1950.g <- merge(seas.pred.post1950[, c("stn", "time", "pred")],   
  grps, all = TRUE)
seas.pred.post1950.g$stn.id <- seas.pred.post1950.g$stn
seas.pred.post1950.g$stn <- seas.pred.post1950.g$group
 
seas.pred.post1950.g <- merge(seas.pred.post1950.g, stn.all)
seas.pred.post1950.g <- ddply(seas.pred.post1950.g, "stn", mutate,
  avg = mean(pred, na.rm = TRUE))

seas.pred.collapsed <- glyphs(seas.pred.post1950.g, "lon", "time", "lat",
  "pred",  width = w,  height = h, y_scale = mean0)

ggplot(seas.pred.collapsed, aes(gx, gy, group = gid)) +
  add_ref_boxes(seas.pred.collapsed, "avg", alpha = 0.2, colour = NA) +
  geom_line(aes(group = stn.id)) +
  theme_fullframe() 
ggsave("../images/usa-season-collapsed.pdf", width = 8, height = 6)

#== gridding
seas.pred.grid <- grid.all(seas.pred.post1950, "lon", "lat", w, h)
glyph.seasonal.grid <- glyphs(seas.pred.grid, "grid.x", "time", "grid.y",   
  "pred", width = 0.95 * w, height = 0.95 * h, y_scale = mean0)

ggplot(glyph.seasonal.grid) +
  add_ref_boxes(glyph.seasonal.grid) +
  geom_line(aes(gx, gy, group = stn)) +
  theme_fullframe() 
ggsave("../images/usa-season-grid.pdf", width = 8, height = 6)

