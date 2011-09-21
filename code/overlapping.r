require(plyr)
get.stn.dists <- function(stn.df, x, y){
  get.one <- function(stn1){
    stn.id <- stn.df[stn1, "stn"]
    stn.pairs <- stn.df[stn.df$stn > stn.id, ]
    data.frame(stn1 = stn.id, stn2 = stn.pairs[ , "stn"],
      stn.df[rep(stn1, nrow(stn.pairs)), c(x, y)] - 
      stn.pairs[ , c(x, y)])
  }
  stn.dists <- ldply(1:(nrow(stn.df) - 1), get.one, .progress = "text")
  names(stn.dists)[3:4] <- paste(names(stn.dists)[3:4], ".dist", sep = "")
  stn.dists
}

overlap <- function(x.dist, y.dist, width, height){
  abs(x.dist) < width & abs(y.dist) < height
}

# dist.df contains results of get.stn.dists (i.e. stn1, stn2 columns)
combine.overlapping <- function(dist.df, x.dists, y.dists, width, height){
  ids <- unique(unlist(dist.df[,c("stn1","stn2")]))
  overlapping <- overlap(dist.df[[x.dists]], dist.df[[y.dists]],
    width, height)
  overlap.df <- subset(dist.df, overlapping)
  overlap.df <- overlap.df[order(overlap.df$stn1), ]
  overlap.ids <- unique(unlist(overlap.df[, c("stn1", "stn2")]))

  groups <- data.frame(stn = overlap.ids, group = NA)
  for (row in 1:nrow(overlap.df)){
    stns <- overlap.df[row, c("stn1", "stn2"), drop = TRUE]
    current.groups <- c(groups[groups$stn == stns[1], "group"], 
      groups[groups$stn == stns[2], "group"])
  
    if (all(is.na(current.groups))){ # if no groups assigned assign to stn1
      groups[groups$stn %in% stns, "group"] <- stns[1]
      # then assign all other stns that overlap with stn1 to stn1
      overlap.with.1 <- unique(unlist(overlap.df[(overlap.df$stn1 == stns[1] |
        overlap.df$stn2 == stns[1]), c("stn1", "stn2")]))
      groups[groups$stn %in% overlap.with.1, "group"] <- stns[1]
    }
  }
  for (row in 1:nrow(overlap.df)){
    stns <- overlap.df[row, c("stn1", "stn2"), drop = TRUE]
    current.groups <- c(groups[groups$stn == stns[1], "group"], 
      groups[groups$stn == stns[2], "group"])
    if (any(is.na(current.groups))){
      ord <- order(current.groups, na.last = TRUE)
      groups[groups$stn == stns[ord[2]], "group"] <- stns[ord[2]] 
      # then add stns that overlap but don't already have groups
      overlap.with.1 <- unique(unlist(
        overlap.df[(overlap.df$stn1 == stns[ord[2]] |
          overlap.df$stn2 == stns[1]), c("stn1", "stn2")]))
      grp.df <- groups[groups$stn %in% overlap.with.1, ]    
      grp.df[is.na(grp.df$group), "group"] <- stns[ord[2]] 
    }
  }  
  not.overlapping <- data.frame(stn = ids[!(ids %in% groups$stn)],
    group = ids[!(ids %in% groups$stn)])
  rbind(groups, not.overlapping)
}

grid.all <- function(data, x, y, width, height){
  data$grid.x <- round_any(data[[x]], width)
  data$grid.y <- round_any(data[[y]], height)
  data
}

abs.dim <- function(size, data, x){
  size / resolution(data[[x]])
}
