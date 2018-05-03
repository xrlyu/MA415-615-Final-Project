library(dplyr)
library(tidyverse)
library(lubridate)

load("./Data/clean.Rdata")

# tidy "found" dataset

found <- found %>% 
  dplyr::select(-c(date, release_date))

found$genres <- gsub("[[:digit:]]+", "", found$genres)
found$genres <- gsub("[[:punct:]]+", "", found$genres)
found$genres <- gsub("id  ", "", found$genres)
found$genres <- gsub("name ", "", found$genres)
found$genres <- gsub(" ", ",", found$genres)
found$genres <- gsub("Science,Fiction", "Science Fiction", found$genres)
found$genres <- gsub("TV,Movie", "TV Movie", found$genres)

# separate the column "genres" so that each row only contain one type of genre
found <- separate_rows(found, genres, sep = "[[:punct:]]+")

found$genres <- ifelse(found$genres == "", NA, found$genres)

# tidy "match" dataset
match <- match %>% dplyr::select(-c(vote_count,date))

# combine two datasets
full <- found %>% bind_rows(match)
full$genres <- as.factor(full$genres)
full$id <- as.character(full$id)

# modify gross box office data to make it adjusted for inflation
full$year <- year(full$release)
full$month <- month(full$release)

inflation <- read.csv("./Data/inflation rate.csv")
names(inflation) <- c("year", "inflation")
inflation$year <- year(inflation$year)
inflation$inflation <- inflation$inflation/ 100

full <- left_join(full, inflation, by = "year") %>% 
  replace_na(list(inflation = 1.182197))

options(scipen = 999)

full$gross <- format(round(full$total_gross * full$inflation))

full <- full %>% 
  arrange(release) %>% 
  dplyr::select(title, id, imdb_id, genres, year, month, gross, vote_average, distributor, days) %>% 
  mutate(vote_average = replace(vote_average, vote_average == 0, NA)) %>% 
  mutate(month = month.abb[month])

saveRDS(full, "./Data/tidy.rds")

# Find information on Oscars Best Pictures

best <- readRDS("./Data/bestpic.rds")
names(best)[1] <- "title"
best_pic <- inner_join(full, best, by = "title") %>% 
  dplyr::select(-date) # find information for movies winning Oscars Best Picture
saveRDS(best_pic, "./Data/bestpic.rds")
