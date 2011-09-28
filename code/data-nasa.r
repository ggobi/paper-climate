if (!file.exists("../data/nasadata.rds")) {
  library("lubridate")

  nasa <- read.csv("../data/nasadata.csv.bz2", stringsAsFactors = FALSE)
  nasa$date <- ymd(nasa$date)
  nasa$day <- (nasa$date - min(nasa$date)) %/% ddays(1)
  nasa$month <- month(nasa$date)
  nasa$year <- year(nasa$date)
  
  saveRDS(nasa, "../data/nasadata.rds")
} else {
  nasa <- readRDS("../data/nasadata.rds")
}
