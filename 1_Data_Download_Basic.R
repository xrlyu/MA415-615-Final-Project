# install.packages("boxoffice")
# load required package for getting box office data
library(boxoffice)
library(dplyr)

# the most recent week of box office data
date <- seq(from = as.Date("2018/04/06"), to = as.Date("2018/04/12"), by = "day")
current <- boxoffice(date = date, site = "numbers")
current$release <- current$date - (current$days-1)

# past 10 years' box office data
date2 <- seq(from = as.Date("2008/1/1"), to = as.Date("2018/04/12"), by = "day" )
total <- boxoffice(date = date2, site = "numbers")
total$release <- total$date - (total$days - 1)

# Academy Award for Best Picture
options(stringsAsFactors = FALSE)
bestpic <- read.delim("./Data/oscars_bestpic.txt", header = FALSE, col.names = "movie")
bestpic$date <- seq(from = 2017, to = 2008, by = -1)

# movie released on or before July 2017
f_movie <- read.csv("./the-movies-dataset/movies_metadata.csv")
f_movie <- f_movie %>% dplyr::select(title, genres, id, imdb_id, release_date, production_companies, vote_average) %>% 
  filter(release_date >= as.Date("2007-01-01"))

# save data as separate RDS files for data cleaning
saveRDS(current, file = "./Data/current.rds")
saveRDS(total, file = "./Data/total.rds")
saveRDS(bestpic, file = "./Data/bestpic.rds")
saveRDS(f_movie, file = "./Data/f_movie.rds")
