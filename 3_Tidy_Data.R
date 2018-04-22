library(dplyr)
library(tidyverse)

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

full <- full %>% 
  dplyr::select(title, id, imdb_id, genres, release, total_gross, vote_average, distributor, days) %>% 
  arrange(release)

# full is the complete list of movie ready for further analysis

saveRDS(full, "./Data/tidy.rds")

