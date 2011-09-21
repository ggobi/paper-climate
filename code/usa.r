library(maps)
library(mgcv)
library(ggplot2)
library(plyr)

source("0-glyphs.r")
source("0-nasa.r")
source("overlapping.r")

# All
temp.all<-read.table("../data/9641C_201012_raw.avg")
colnames(temp.all)<-c("year", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec", "ann")
temp.all$stn<-temp.all$year%/%100000
temp.all$year<-temp.all$year%%10000

stn.all<-read.table("../data/ushcn-stations.txt", fill=T)
stn.all<-stn.all[,c(1:3,5:6)]
colnames(stn.all)<-c("stn", "lat", "lon", "state", "town")
stn.all$name<-paste(stn.all$town, stn.all$state)

# Post 1980
temp.post1980 <- subset(temp.all, year >= 1980)
temp.post1980 <- merge(temp.post1980, stn.all, by="stn")

temp.post1980.melt <- melt(temp.post1980[,-c(15, 19)], 
  id=c("stn", "year", "lat", "lon", "name", "state"))
temp.post1980.melt <- rename(temp.post1980.melt, 
    c("variable" = "month", "value" = "temp"))

season_index <- function(time){
  (time %% 12  + 1 )/ 12
}

temp.post1980.melt <- mutate(temp.post1980.melt, 
    temp = temp/10,
    date = as.Date(paste(year, "-", as.numeric(month), "-01", sep = "")),
    time = (year - 1980) * 12 + as.numeric(month),
    season = season_index(time)
    )    
temp.post1980.melt$temp[temp.post1980.melt$temp == -999.9] = NA

#==== Modelling

seasonal.fit.post1980 <- dlply(temp.post1980.melt, "stn", function(df){
  gam(temp ~ s(season, bs = "cc"), 
    knots = list(season = c(0, 12)),
    data = df, na.action = "na.exclude")
  }, .progress = "text"
) 

temp.post1980.melt <- cbind(temp.post1980.melt, 
  ldply(seasonal.fit.post1980, function(fit) {
    data.frame(res = residuals(fit), pred = predict(fit))},
  .progress = "text"))

pred.seas.post1980 <- ldply(seasonal.fit.post1980, function(fit){
    one.df <- data.frame(time = 1:24)
    one.df <- mutate(one.df, season = season_index(time))
    one.df$pred <- predict(fit, one.df)
    one.df$intercept <- coef(fit)[1]
    one.df
  }, .progress = "text")


pred.seas.post1980 <- ddply(pred.seas.post1980, "stn", mutate, avg = mean(pred), pred.cent = pred - avg)
pred.seas.post1980 <- merge(pred.seas.post1980, stn.all)

change.fit.post1980 <- dlply(temp.post1980.melt, "stn", function(df){
  loess(res ~ time, data = df)
  }, .progress = "text"
) 

pred.change.post1980 <- ldply(change.fit.post1980, function(fit){
    one.df <- data.frame(
      time = seq(min(fit$x, na.rm = TRUE), max(fit$x, na.rm = TRUE), 12))
    one.df$pred <- predict(fit, one.df)
    one.df
  }, .progress = "text")
pred.change.post1980 <- merge(pred.change.post1980, stn.all)

warm.change.post1980 <- ddply(pred.change.post1980, "stn", summarise,
  warmer = pred[time == min(time)] < pred[time == max(time)],
  .progress = "text")


#==== Plotting

# resolution doesn't really work non gridded data
# it looks at univariate distances, but really we want bivarite ones
# euclidean distance isn't quite right either, becasue we want rectangular(?) 
# icons
stn.dists <- get.stn.dists(stn.all, "lon", "lat")

h <- 1
w <- 2

# the overlapping problem
glyph.seasonal <- glyphs(pred.seas.post1980, "lon", "time", "lat", "pred.cent",
  width = abs.dim(w, pred.seas.post1980, "lon"),
  height = abs.dim(h, pred.seas.post1980, "lat")
)
ggplot(glyph.seasonal) +
  geom_line(aes(gx, gy, group = stn)) +
  theme_fullframe()
ggsave("../images/usa-overlapping.pdf", width = 8, height = 6)

#== shrinking glyphs isn't an option here, zooming is...

#== one solution combine overlapping glyphs
grps <- combine.overlapping(stn.dists, "lon.dist", "lat.dist", w, h)

pred.seas.post1980.g <- merge(pred.seas.post1980[, c("stn", "time", "pred", "pred.cent")],   
  grps, all = TRUE)

pred.seas.post1980.collapse <- pred.seas.post1980.g
pred.seas.post1980.collapse$stn.id <- pred.seas.post1980.collapse$stn
pred.seas.post1980.collapse$stn <- pred.seas.post1980.collapse$group
 
pred.seas.post1980.collapse <- merge(pred.seas.post1980.collapse, stn.all)

glyph.seasonal.col <- glyphs(pred.seas.post1980.collapse, "lon", "time", "lat", "pred.cent",   
  width = abs.dim(0.8 * w, pred.seas.post1980.collapse, "lon"),
  height = abs.dim(0.8 * h, pred.seas.post1980.collapse, "lat")
  )

ggplot(glyph.seasonal.col) +
  geom_line(aes(gx, gy, group = stn.id)) +
  theme_fullframe() + 
  geom_tile(height = h/2, width= w/2, colour = "white", fill = NA) +
  geom_line(aes(gx, gy, group = stn)) 
ggsave("../images/usa-collapsed.pdf", width = 8, height = 6)
  
#== another solution gridding
pred.seas.post1980.grid <- grid.all(pred.seas.post1980, "lon", "lat", w, h)
glyph.seasonal.grid <- glyphs(pred.seas.post1980.grid, "grid.x", "time", "grid.y", "pred.cent",   
  width = abs.dim(0.8 * w, pred.seas.post1980.grid, "grid.x"),
  height = abs.dim(0.8 * h, pred.seas.post1980.grid, "grid.y")
  )

ggplot(glyph.seasonal.grid) +
  geom_line(aes(gx, gy, group = stn)) +
  theme_fullframe() 
ggsave("../images/usa-grid.pdf", width = 8, height = 6)

