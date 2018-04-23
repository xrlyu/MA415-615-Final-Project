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

# separate the column "genres" so that each row only contain one type of genre
found <- separate_rows(found, genres)

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
full$day <- 1

inflation <- read.csv("./Data/inflation rate.csv")
names(inflation) <- c("year", "inflation")
inflation$year <- year(inflation$year)
inflation$inflation <- inflation$inflation/ 100
full <- left_join(full, inflation, by = "year") %>% 
  replace_na(list(inflation = 1.182197))

full$gross <- full$total_gross * full$inflation
full <- full %>% unite(date, year, month, day, sep = "-") %>% 
  dplyr::select(-c(release, inflation, total_gross)) %>% 
  dplyr::select(title, id, imdb_id, genres, date, gross, vote_average, distributor, days) %>% 
  arrange(date)

saveRDS(full, "./Data/tidy.rds")

