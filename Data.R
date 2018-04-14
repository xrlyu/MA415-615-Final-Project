# install.packages("boxoffice")
# load required package for getting box office data
library(boxoffice)

# the most recent week of box office data
date <- seq(from = as.Date("2018/04/06"), to = as.Date("2018/04/12"), by = "day")
current <- boxoffice(date = date, site = "numbers")

# past 10 years' box office data
date2 <- seq(from = as.Date("2008/1/1"), to = as.Date("2018/04/12"), by = "day" )
historical <- boxoffice(date = date2, site = "numbers")

# Academy Award for Best Picture
options(stringsAsFactors = FALSE)
bestpic <- read.delim("oscars_bestpic.txt", header = FALSE, col.names = "movie")
bestpic$date <- seq(from = 2017, to = 2008, by = -1)

# save data as separate RDS files for data cleaning
saveRDS(current, file = "./Data/current.rds")
saveRDS(historical, file = "./Data/historical.rds")
saveRDS(bestpic, file = "./Data/bestpic.rds")


## now work with API to dowonload details of movies
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("lubridate")

library('httr')
library("jsonlite")
library('lubridate')

## api
## 2c2d3eeeb39c7d6d18608fbdcd6407ad

test <- GET("https://api.themoviedb.org/3/search/movie?api_key=2c2d3eeeb39c7d6d18608fbdcd6407ad&query=Jack+Reacher")

names(test)

test$status_code

this.raw.content <- rawToChar(test$content)

this.content <- fromJSON(this.raw.content)
