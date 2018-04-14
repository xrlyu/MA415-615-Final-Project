# install.packages("boxoffice")
# load required package for getting box office data
library(boxoffice)

# the most recent week of box office data
date <- seq(from = as.Date("2018/04/06"), to = as.Date("2018/04/12"), by = "day")
current <- boxoffice(date = date, site = "numbers")
current$release <- current$date - (current$days-1)

# past 10 years' box office data
date2 <- seq(from = as.Date("2008/1/1"), to = as.Date("2018/04/12"), by = "day" )
historical <- boxoffice(date = date2, site = "numbers")
historical$release <- historical$date - (historical$days - 1)

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

library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

# transform the list of movies
movie <- historical %>% select(movie, release) %>% distinct() # a list of movies released in the past decade with release date
m_list <- sapply(movie[,1],function(x) gsub("[[:punct:] ]+"," ", x))
m_list <- sapply(m_list, function(x) gsub(" ","+", x)) # for API

# write a function to get information from the API

get_api <- function(movie_list) {
  m_total <- data.frame()
  for (i in 1:length(movie_list)) {
    dataset <- GET(paste("https://api.themoviedb.org/3/search/movie?api_key=2c2d3eeeb39c7d6d18608fbdcd6407ad&query=", movie_list[i], sep=""))
    dataset$status_code
    this.raw.content <- rawToChar(dataset$content)
    this.content <- fromJSON(this.raw.content)
    content <- this.content$results
    m_total <- rbind(m_total,content)
  }
  m_total <<- m_total
}

get_api(m_list)

# save movie information to RData file for cleaning

rm(current)
rm(historical)
rm(bestpic)
rm(m_list)

save.image(file = "./Data/movie_list.RData")


## api
## 2c2d3eeeb39c7d6d18608fbdcd6407ad

test <- GET("https://api.themoviedb.org/3/search/movie?api_key=2c2d3eeeb39c7d6d18608fbdcd6407ad&query=Juno")

## use this to get TMdb id for each movie
names(test)

test$status_code

this.raw.content <- rawToChar(test$content)

this.content <- fromJSON(this.raw.content)

content <- this.content$results

