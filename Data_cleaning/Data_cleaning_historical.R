library(dplyr)
library(tidyverse)
library(stringr)
library(fuzzyjoin)

# import the file with API function
source("reference.R")

# NOTE: Kaggle dataset in this file refers to the modified version of the file "movies_metadata.csv" which was downloaded from Kaggle

# read data
total <- readRDS("./Data/total.rds")
f_movie <- readRDS("./Data/f_movie_1.rds")

# a complete list of movies released after 2008-01-01 (according to box office data)
total <- total %>% filter(release >= as.Date("2008-01-01")) %>% 
  dplyr::select(c(movie, distributor, release, date, total_gross, days)) %>% 
  arrange(release, movie) %>% 
  group_by(movie, distributor, release) %>% 
  top_n(1, days)

# change some properties for the list of movies
names(total)[1] <- 'title'
total <- as.data.frame(total)

# some modifications for Kaggle dataset
f_movie$production_companies <- gsub("'name':", "", f_movie$production_companies)
f_movie$production_companies <- gsub("'id':", "", f_movie$production_companies)
f_movie$production_companies <- gsub("[[:digit:]]+", "", f_movie$production_companies)
f_movie$production_companies <- gsub("'", "", f_movie$production_companies)
f_movie$production_companies <- gsub("\\[|\\]", "", f_movie$production_companies)
f_movie$production_companies <- gsub("\\{ ", "", f_movie$production_companies)

# join the list of movies and Kaggle dataset together
w_id <- left_join(total, f_movie, by = "title")

###########################

# find out duplicated values (movies with the same titile or TMbd id) in merged dataset
duplicate <- filter(w_id, duplicated(w_id$title) == TRUE) %>%  
  dplyr::select(title) %>% distinct() %>% inner_join(w_id, by = "title") %>% distinct() %>% 
  arrange(title, release)

# Kaggle dataset only has information for movies released on or before July 2017. To ensure the accuracy
# of data, I would like to find out information for movies released on or after January 2017, and in this case,
# I categorize these movies under another dataset named "missing". I need to find information for these movies through API later on.
missing <- w_id %>% filter(release >= as.Date("2017-01-01")) %>% 
  dplyr::select(title, distributor, release, date, total_gross, days) %>% 
  distinct()

# two movies, "Is Genesis History?" and "The Last Dalai Lama?" need manual corrections because of gaps between two showing periods
missing <- missing %>% filter((title != "The Last Dalai Lama?") | (release != as.Date("2017-07-08"))) %>% 
  filter((title != "Is Genesis History?") | (release != as.Date("2017-02-24")))

copy <- missing # save a copy of the list for future use

# update the list of duplicated movies by excluding movies released on or after 2017-01-01
duplicate <- duplicate %>% filter(release < as.Date("2017-01-01"))

copy <- copy %>% bind_rows(duplicate) # save a copy of the list for future use

# I first attempt to find exact movies through matching production companies
found <- duplicate %>% 
  filter(str_detect(duplicate$production_companies,duplicate$distributor) == TRUE) %>% 
  dplyr::select(-c(production_companies))

# a quick examination of the list shows that there is still one problem needed to be manually fixed
# the problem is associated with a movie "The Walk" on show in theater for an extra day
# I manually replace the original release date caculated based on box office data for "The Walk" after that extra day 
# and remove the unnecessary row
found[31,3] <- found[30,3]
found <- found %>% filter((title != "The Walk")|(date != "2015-10-22"))

# update the list of duplicated values
duplicate <- duplicate[!(duplicate$title %in% found$title),]

###########################
# now attempt to find exact values for other duplicated values
# I try to match movies by comparing the release date calcaulatd by the box office data and the release date reported in Kaggle dataset
duplicate$diff <- abs(difftime(duplicate$release, duplicate$release_date))

found2 <- duplicate %>% group_by(title, distributor) %>% 
  top_n(-1, diff)

# after a quick examination of the dataset, I found out there are two movies "Priceless" and "Insiduous" that need special attention
# since there are two movies released under the same name but TMdb only has data for one of them
# have to make modifications mannually
# I treated them as missing values

found2 <- found2 %>% filter((title != "Priceless") | (release != as.Date('2008-03-28')))  %>% 
  filter((title != "Insidious") | (release != as.Date("2008-05-23"))) %>% 
  dplyr::select(-c(production_companies, diff))

# combine this list with the list "found" and update the list to include two missing movies
found <- found %>% bind_rows(found2)
rm(found2)

found <- found %>% bind_rows(filter(total, (title == "Priceless") & (release == as.Date('2008-03-28')))) %>% 
  bind_rows(filter(total, (title == "Insidious") & (release == as.Date('2008-05-23'))))

# update the list of duplicated values
duplicate <- duplicate[!(duplicate$title %in% found$title),]

# it turns out that there is no matching value found in Kaggle dataset for the duplicated list
# so I manually modified the list and make it another missing list - "missing2" that requires additional information
duplicate[4,3] <- duplicate[3,3]
duplicate[4,6] <- 20
duplicate[10,3] <- duplicate[9,3]
duplicate[12,3] <- duplicate[11,3]
duplicate[24,3] <- duplicate[23,3]
missing2 <- duplicate %>% filter((title != "Water & Power")|(date != "2014-04-12")) %>% 
  filter((title != "Until Forever") | (days == 299)) %>% 
  filter((title != "The Twilight Saga: Breaking…") | (days != 1)) %>% 
  filter((title != "The Hornet’s Nest") | (date == as.Date("2014-07-31"))) %>% 
  filter((title != "Spices of Liberty") | (date == as.Date("2017-03-12"))) %>% 
  filter((title != "Not Today") | (date == as.Date("2013-07-25"))) %>% 
  filter((title != "Caged No More") | (date == as.Date("2016-02-11"))) %>% 
  dplyr::select(-c(diff, genres, id, imdb_id, release_date, production_companies, vote_average))

rm(duplicate)

###########################
# update the list of movies that are found in the Kaggle dataset
found <- w_id %>% filter(is.na(id) == FALSE) %>% 
  anti_join(copy, by = c("title", "distributor", "release")) %>% 
  dplyr::select(-c(production_companies)) %>% 
  bind_rows(found)

found$id <- as.integer(found$id)

###########################
# list of movies released before 2017-01-01 and are not in Kaggle dataset that need additional information
missing3 <- w_id %>% filter(release < as.Date("2017-01-01")) %>% 
  filter(is.na(id) == TRUE) %>% 
  dplyr::select(title, distributor, release, date, total_gross, days)

###########################
# download movie information
# first for the list of movies in "missing"
m_list <- sapply(missing$title,function(x) gsub("[[:punct:] ]+"," ", x)) # replace punctuations with blank space
m_list <- sapply(m_list, function(x) gsub(" ","+", x)) # insert "+" between words

# break down the large list for the convenience of downloading data (there is rate limit for downloads)
for (i in 1:(length(m_list) %/% 40)) {
  a <- (i-1)*40+1
  b <- i*40
  assign(paste('m_list',i,sep='_'), m_list[a:b])
}

assign(paste('m_list', length(m_list) %/% 40 + 1, sep = "_"), m_list[(length(m_list) %/% 40 *40 + 1):length(m_list)])

# need to manually execute these codes to ensure that only 40 observations are searched for each 10sec interval
# get_api(m_list_1)
# get_api(m_list_2)
# get_api(m_list_3)
# get_api(m_list_4)
# get_api(m_list_5)
# get_api(m_list_6)
# get_api(m_list_7)
# get_api(m_list_8)
# get_api(m_list_9)
# get_api(m_list_10)

missing_info <- m_info

saveRDS(missing_info, "./Data/missing_info.rds")

# clean the list of movies first
missing_info <- missing_info %>% 
  dplyr::select(title, id, original_title, release_date) %>% 
  distinct() %>% 
  filter(release_date != "") %>% 
  filter(release_date >= as.Date("2015-01-01"))

# eliminate "…" in the title of movies to facilitate matching
missing$title <- gsub("…", "", missing$title)

# join "missing" and "missing_info" together
w_id <- regex_right_join(missing_info, missing, by = c(title = "title"), ignore_case = TRUE) %>% 
  distinct()

w_id$diff <- abs(difftime(w_id$release, w_id$release_date))

need_info <- w_id %>% 
  group_by(title.y, distributor, release) %>% 
  top_n(-1, diff) %>% 
  ungroup() %>% 
  dplyr::select(title.x, title.y, id, release, distributor, date, total_gross, days)

# the movie "Breathe" is associated with two different ids in TMdb database. need to find out the correct one manually
need_info <- need_info %>% filter((title.x != "Breathe") | (id != 495179))

# manually remove the incorrect row - for the movie "Kingsman: The Golden Circle"
# note: regex_join joins by partial matching of strings. However, we need exact match for this one observation, and we manually remove it just for convenience
need_info <- need_info %>% filter((title.x != "Kingsman: The Golden Circle") |(title.y != "Gold"))

# further clean up the list
need_info <- need_info %>% dplyr::select(-c(title.y))

# there still are movies that cannot be found
missing.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, release, distributor, date, total_gross, days)
names(missing.1)[1] <- "title" # change the column name

# attempt to match missing values 
# remove any punctuation in the title of movies in both lists - "missing.1" and "missing_info" and replace with nubmers (for the convenience of redo)
missing_info$title <- gsub("'", "3", missing_info$title)
missing.1$title <- gsub("’", "3", missing.1$title)

missing_info$title <- gsub(":", "9", missing_info$title)
missing.1$title <- gsub(":", "9", missing.1$title)

missing_info$title <- gsub("!", "7", missing_info$title)
missing.1$title <- gsub("!", "7", missing.1$title)

missing_info$title <- gsub("\\$", "6", missing_info$title)
missing.1$title <- gsub("\\$", "6", missing.1$title)

# then match these two lists again
w_id <- regex_right_join(missing_info, missing.1, by = c(title = "title"), ignore_case = TRUE)

# fixing title of movies
w_id$title.x <- gsub("3", "'", w_id$title.x)
w_id$title.y <- gsub("3", "'", w_id$title.y)

w_id$title.x <- gsub("9", ":", w_id$title.x)
w_id$title.y <- gsub("9", ":", w_id$title.y)

w_id$title.x <- gsub("7", "!", w_id$title.x)
w_id$title.y <- gsub("7", "!", w_id$title.y)

w_id$title.x <- gsub("6", "$", w_id$title.x)
w_id$title.y <- gsub("6", "$", w_id$title.y)

# update the list of movies that have been found
need_info <-  w_id %>% filter(is.na(id) == FALSE) %>% 
  dplyr::select(title.x, id, release, distributor, date, total_gross, days) %>% 
  bind_rows(need_info)

# update the list of movies that are still missing
missing.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, release, distributor, date, total_gross, days)
names(missing.1)[1] <- "title" # change the column name

# another attemp to match movies: see if movies are in a foreign name (use original title)
# also similar titles are modified to facilitate matching
missing.1$title <- gsub("John Wick: Chapter Two", "John Wick: Chapter 2", missing.1$title)

w_id <- regex_right_join(missing_info, missing.1, by = c(original_title = "title"), ignore_case = TRUE)
w_id$title.x <- gsub("9", ":", w_id$title.x)

# update the list of movies that have been found
need_info <- w_id %>% filter(is.na(id) == FALSE) %>% 
  dplyr::select(title.x, id, release, distributor, date, total_gross, days) %>% 
  bind_rows(need_info)

# update the list of movies that are still missing
missing.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, release, distributor, date, total_gross, days)
names(missing.1)[1] <- "title" # change the column name

# one movie "Padmavati" has the wrong name so I manually changed the title of the movie
# the name of the movie should be "Padmaavat"
# then get info for this movie from TMdb database
missing.1$title <- gsub("Padmavati", "Padmaavat", missing.1$title)
m_info <- data.frame()
get_api("Padmaavat")

# update the list of movies that have been found
need_info <- m_info %>% dplyr::select(title, id) %>% 
  inner_join(missing.1) %>% 
  bind_rows(need_info)

# update the list of movies that are still missing
missing.1 <- missing.1 %>% filter(title != "Padmaavat")

# remove "&" in the title of movies in the two lists with "and"
missing.1$title <- gsub("&", "and", missing.1$title)
missing_info$title <- gsub("&", "and", missing_info$title)

# match two lists again
w_id <- regex_right_join(missing_info, missing.1, by = c(title = "title"), ignore_case = TRUE)
w_id$title.x <- gsub("Victoria and Abdul", "Victoria & Abdul", w_id$title.x)

# update the list of movies that have been found
need_info <- w_id %>% filter(is.na(id) == FALSE) %>% 
  dplyr::select(title.x, id, release, distributor, date, total_gross, days) %>% 
  bind_rows(need_info)

# update the list of movies that are still missing
missing.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, release, distributor, date, total_gross, days)
names(missing.1)[1] <- "title" # change the column name

# two movies are found in Kaggle dataset but with a slightly different name
# movie titles are manually modified to facilitate matching process
missing.1$title <- gsub("T2: Trainspotting", "T2 Trainspotting", missing.1$title)
missing.1$title <- gsub("Long Strange Trip: The Unto", "Long Strange Trip", missing.1$title)
missing.1$title <- gsub("Tyler Perry's Boo 2! A Ma", "Boo! A Madea Halloween", missing.1$title)

# join two datasets to match movies 
w_id <- inner_join(missing.1, f_movie) %>% 
  dplyr::select(-c(production_companies)) 
w_id$id <- as.integer(w_id$id)

# update the list of movies with complete information
found <- found %>% bind_rows(w_id)

# update the list of movies that are still missing
missing.1 <- anti_join(missing.1, found)

# modify titles of movies that have information in TMdb dataset to faciliate the merging process
missing.1$title <- gsub("Guardians of the Galaxy Vol 2", "Guardians of the Galaxy", missing.1$title)
missing.1$title <- gsub("The Wedding Party 2: Destin", "The Wedding Party 2", missing.1$title)
missing.1$title <- gsub("Star Wars Ep. VIII: The Las", "Star Wars The Last Jedi", missing.1$title)
missing.1$title <- gsub("‘85 The Greatest Team in", "85 The Greatest Team", missing.1$title)

w_id <- regex_right_join(missing_info, missing.1, by = c(original_title = "title"), ignore_case = TRUE)

# update the list of movies that have been found
need_info <- w_id %>% filter(is.na(id) == FALSE) %>% 
  dplyr::select(title.x, id, release, distributor, date, total_gross, days) %>% 
  bind_rows(need_info)

# update the list of movies that are still missing
missing.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, release, distributor, date, total_gross, days)
names(missing.1)[1] <- "title" # change the column name

# fix title of movies in the list "missing_info"
missing_info$title <- gsub("3", "'", missing_info$title)
missing_info$title <- gsub("9", ":", missing_info$title)
missing_info$title <- gsub("[[:punct:] ]+", " ", missing_info$title)
missing_info$title <- gsub("85 The Greatest Team in Pro Football History", "85 The Greatest Team", missing_info$title)
missing_info$original_title <- gsub("120 battements par minute", "BPM (Beats per Minute)", missing_info$original_title)

# match two lists
w_id <- regex_right_join(missing_info, missing.1, by = c(title = "title"), ignore_case = TRUE)

# update the list of movies that have been found
need_info.1 <- w_id %>% filter(is.na(id) == FALSE) %>% 
  dplyr::select(original_title, id, release, distributor, date, total_gross, days)
names(need_info.1)[1] <- "title.x"
need_info <- need_info %>% bind_rows(need_info.1)

# update the list of movies that are still missing
missing.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, release, distributor, date, total_gross, days)
names(missing.1)[1] <- "title" # change the column name

# find the last matching value - need to do this manually because for unknown reason R cannot merge two datasets correctly
greatest.team <- missing_info[str_which(missing_info$title,"85 The Greatest Team"),]
w_id <- bind_rows(greatest.team, missing.1)
w_id[7,1] <- w_id[1,3]
w_id[7,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
rm(greatest.team)

# update the list of movies that need additional information
need_info.1 <- w_id %>% filter(is.na(id) == FALSE)
names(need_info.1)[1] <- "title.x"
need_info <- need_info %>% bind_rows(need_info.1)
rm(need_info.1)

# update the list of movies that are still missing
missing.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(-id)

# again, because regex_join joins by partial matching of strings and one observation is excluded erroneously
# thus we include this missing value together with the list "missing.1"
# there is no information for the movie "Gold" distributed by "Sony Pictures"

missing.1 <- missing.1 %>% bind_rows(filter(missing, title == "Gold" & distributor == "Sony Pictures"))

# include missing data and update the list of movies that have been found
found <- found %>% full_join(missing.1)

need_info <- need_info %>% replace_na(list(title.x = "", title = "")) %>% 
  unite(title, title.x, title, sep = "")

rm(missing)
rm(missing_info)
rm(missing.1)

###########################
# then for the list of movies in "missing2"
m_list <- sapply(missing2$title,function(x) gsub("[[:punct:] ]+"," ", x))
m_list <- sapply(m_list, function(x) gsub(" ","+", x))

# reset the varaible
m_info <- data.frame()

# get_api(m_list)

missing2_info <- m_info

saveRDS(missing2_info, "./Data/missing2_info.rds")

# clean the list of movies first
missing2_info <- missing2_info %>% 
  dplyr::select(title, id, original_title, release_date) %>% 
  arrange(release_date, title) %>% distinct() %>% 
  filter(release_date != "") %>% 
  filter(release_date >= as.Date("2008-01-01"))

# eliminate "…" in the title of movies to facilitate matching
missing2$title <- gsub("…", "", missing2$title)

# join the "missing2" and the list of movies "missing2_info" together 
w_id <- regex_right_join(missing2_info, missing2, by = "title", ignore_case = FALSE)

# match movies based on release date
w_id$diff <- abs(difftime(w_id$release, w_id$release_date))

# movies with found id in TMdb databast. need to use the id to get additional information for movies
need_info2 <- w_id %>% group_by(title.y, distributor, release) %>% top_n(-1, diff) %>% 
  ungroup() %>% 
  dplyr::select(title.x, id, release, distributor, date, total_gross, days)

names(need_info2)[1] <- "title" # change the column name

# two movies are not found in TMdb database
missing2.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, release, distributor, date, total_gross, days)
names(missing2.1)[1] <- "title" # change the column name

# update the list of movies with complete information
found <- found %>% bind_rows(missing2.1)

rm(missing2)
rm(missing2_info)
rm(missing2.1)

###########################
# find information for the list of movies in "missing3"
m_list <- sapply(missing3$title,function(x) gsub("[[:punct:] ]+"," ", x))
m_list <- sapply(m_list, function(x) gsub(" ","+", x))

# break down the large list for the convenience of downloading data (there is rate limit for downloads)
for (i in 1:(length(m_list) %/% 40)) {
  a <- (i-1)*40+1
  b <- i*40
  assign(paste('m_list',i,sep='_'), m_list[a:b])
}

assign(paste('m_list', length(m_list) %/% 40 + 1, sep = "_"), m_list[(length(m_list) %/% 40 *40 + 1):length(m_list)])

# reset the variable
m_info <- data.frame()

# need to manually execute these codes to ensure that only 40 observations are searched for each 10sec interval
# get_api(m_list_1)
# get_api(m_list_2)
# get_api(m_list_3)
# get_api(m_list_4)
# get_api(m_list_5)
# get_api(m_list_6)
# get_api(m_list_7)
# get_api(m_list_8)
# get_api(m_list_9)
# get_api(m_list_10)
# get_api(m_list_11)
# get_api(m_list_12)
# get_api(m_list_13)
# get_api(m_list_14)
# get_api(m_list_15)
# get_api(m_list_16)

# clean the list of movies first 
missing3_info <- missing3_info %>% 
  dplyr::select(title, id, original_title, release_date) %>% 
  arrange(release_date, title) %>% distinct() %>% 
  filter(release_date != "") %>% 
  filter(release_date >= as.Date("2005-01-01")) %>% 
  filter(release_date < as.Date('2018-01-01'))

saveRDS(missing3_info, "./Data/missing3_info.rds")

# first by exact matching
w_id <- left_join(missing3, missing3_info, by = "title")

# movies that need additional information from TMdb database
need_info3 <- w_id %>% filter(is.na(id) == FALSE) %>% 
  arrange(title)

# filter incorrect matches (one movie corresponding to multiple ids)
need_info3$diff <- abs(difftime(need_info3$release, need_info3$release_date))
need_info3 <- need_info3 %>% 
  group_by(title, distributor, release) %>% 
  top_n(-1, diff)

# we have found ids for four movies in the list when cleaning the list "missing_1" so we remove them from the list "need_info3"
need_info3 <-need_info3 %>%  
  filter(title != "Caged No More") %>% 
  filter(title != "Not Today") %>% 
  filter(title != "Until Forever") %>% 
  filter(title != "Water & Power") %>% 
  dplyr::select(-c(diff, release_date, original_title))

# movies that are still missing
missing3.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(-c(original_title, id, release_date))

# eliminate "…" in the title of movies to facilitate matching
missing3.1$title <- gsub("…", "", missing3.1$title)
missing3.1$title <- gsub("[[:punct:] ]+"," ", missing3.1$title)

# partial match mess up movies with extremely similar titles
# manually replace potential incorrect matches
missing3_info <- missing3_info %>% filter(title != "The Disappearance of Eleanor Rigby: Her") %>% 
  filter(title != "The Disappearance of Eleanor Rigby: Him")

# then do the matching again (string partial match)
w_id <- regex_right_join(missing3_info, missing3.1, by = "title", ignore_case = TRUE)

# update the list of movies that additional information
# filter incorrect matches
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$diff <- abs(difftime(need_info3.1$release, need_info3.1$release_date))

need_info3.1 <- need_info3.1 %>% group_by(title.y, distributor, release) %>% 
  top_n(-1, diff) %>% 
  ungroup() %>% 
  dplyr::select(c(title.x, distributor, release, date, total_gross, days, id)) %>% 
  arrange(title.x)
names(need_info3.1)[1] <- 'title'
need_info3 <- need_info3 %>% bind_rows(need_info3.1)

# movies that are still missing
missing3.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, distributor, release, date, total_gross, days)
names(missing3.1)[1] <- "title"

missing_info_2 <- missing3_info
missing_info_2$title <- gsub("[[:punct:] ]+"," ", missing_info_2$title)

w_id <- regex_right_join(missing_info_2, missing3.1, by = "title", ignore_case = TRUE)

# update the list of movies that need additional info
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$diff <- abs(difftime(need_info3.1$release, need_info3.1$release_date))
need_info3.1 <- need_info3.1 %>% group_by(title.y, distributor, release) %>% 
  top_n(-1, diff) %>% 
  filter((title.x != "The Twilight Saga Breaking Dawn Part 2") | (days != 1)) %>%
  filter(title.x != "The Hornet s Nest")  %>% # we have cleaned these two movies before
  ungroup() %>% 
  dplyr::select(c(original_title, distributor, release, date, total_gross, days, id))

need_info3 <- left_join(need_info3.1, missing3_info, by = "id") %>% 
  dplyr::select(c(title, distributor, release, date, total_gross, days, id)) %>% 
  bind_rows(need_info3)
  
# update the list of movies that are still missing
missing3.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(title.y, distributor, release, date, total_gross, days)
names(missing3.1)[1] <- "title"

# there are errors in movies titles and in order to facilitate the matching process, I manually corrected these errors
missing3.1$title <- gsub("10 000 B C", "10,000 BC", missing3.1$title)
missing3.1$title <- gsub("The Flight of the Red Balloon", "Flight of the Red Balloon", missing3.1$title)
missing3.1$title <- gsub("And When Did You Last See Y", "When Did You Last See Your Father?", missing3.1$title)
missing3.1$title <- gsub("The Girl Cut in Two", "A Girl Cut in Two", missing3.1$title)
missing3.1$title <- gsub("Underworld 3 Rise of the L", "Underworld: Rise of the Lycans", missing3.1$title)
missing3.1$title <- gsub("Crank 2 High Voltage", "Crank: High Voltage", missing3.1$title)
missing3.1$title <- gsub("Merry Gentleman The", "The Merry Gentleman", missing3.1$title)
missing3.1$title <- gsub("Call of the Wild 3D", "Call of the Wild", missing3.1$title)
missing3.1$title <- gsub("The Taking of Pelham 123", "The Taking of Pelham 1 2 3", missing3.1$title)
missing3.1$title <- gsub("Michael Jackson s This Is It", "This Is It", missing3.1$title)
missing3.1$title <- gsub("The Boondock Saints 2 All", "The Boondock Saints II: All Saints Day", missing3.1$title)
missing3.1$title <- gsub("Disney s A Christmas Carol", "A Christmas Carol", missing3.1$title)
missing3.1$title <- gsub("Precious Based on the Nove", "Precious", missing3.1$title)
missing3.1$title <- gsub("The Fantastic Mr Fox", "Fantastic Mr. Fox", missing3.1$title)
missing3.1$title <- gsub("District B13 Ultimatum", "District 13: Ultimatum", missing3.1$title)
missing3.1$title <- gsub("Prince of Persia Sands of", "Prince of Persia: The Sands of Time", missing3.1$title)
missing3.1$title <- gsub("Coco Chanel and Igor Stravinsky", "Coco Chanel & Igor Stravinsky", missing3.1$title)
missing3.1$title <- gsub("Mesrine Public Enemy No 1", "Mesrine: Public Enemy #1", missing3.1$title)
missing3.1$title <- gsub("Alpha and Omega 3D", "Alpha and Omega", missing3.1$title)
missing3.1$title <- gsub("Wall Street 2 Money Never ", "Wall Street: Money Never Sleeps", missing3.1$title)
missing3.1$title <- gsub("Ip Man 2 Legend of the Gra", "Ip Man 2", missing3.1$title)
missing3.1$title <- gsub("Born to be Wild 3D", "Born to be Wild", missing3.1$title)
missing3.1$title <- gsub("Atlas Shrugged Part 1", "Atlas Shrugged: Part I", missing3.1$title)
missing3.1$title <- gsub("POM Wonderful Presents The", "The Greatest Movie Ever Sold", missing3.1$title)
missing3.1$title <- gsub("HEY BOO Harper Lee and To", "Hey, Boo: Harper Lee & To Kill a Mockingbird", missing3.1$title)
missing3.1$title <- gsub("Last Mountain The", "The Last Mountain", missing3.1$title)
missing3.1$title <- gsub("Glee The 3D Concert Movie", "Glee: The Concert Movie", missing3.1$title)
missing3.1$title <- gsub("Shark Night 3D", "Shark Night", missing3.1$title)
missing3.1$title <- gsub("A Very Harold Kumar 3D Ch", "A Very Harold & Kumar Christmas", missing3.1$title)
missing3.1$title <- gsub("Tyler Perry s Good Deeds", "Good Deeds", missing3.1$title)
missing3.1$title <- gsub("To the Arctic 3D", "To the Arctic", missing3.1$title)
missing3.1$title <- gsub("Tyler Perry s Madea s Witne", "Madea's Witness Protection", missing3.1$title)
missing3.1$title <- gsub("The War of the Buttons", "War of the Buttons", missing3.1$title)
missing3.1$title <- gsub("The Met Live in HD Aida", "Aida", missing3.1$title)
missing3.1$title <- gsub("21 and Over", "21 & Over", missing3.1$title)
missing3.1$title <- gsub("Tyler Perry s Temptation", "Temptation: Confessions of a Marriage Counselor", missing3.1$title)
missing3.1$title <- gsub("Free Angela All Political", "Free Angela and All Political Prisoners", missing3.1$title)
missing3.1$title <- gsub("5 de Mayo La Batalla", "Cinco de Mayo: La Batalla", missing3.1$title)
missing3.1$title <- gsub("The Hangover 3", "The Hangover Part III", missing3.1$title)
missing3.1$title <- gsub("Fast and Furious 6", "Fast & Furious 6", missing3.1$title)
missing3.1$title <- gsub("Disney Planes", "Planes", missing3.1$title)
missing3.1$title <- gsub("Lee Daniels The Butler", "The Butler", missing3.1$title)
missing3.1$title <- gsub("Jackass Presents Bad Grandpa", "Bad Grandpa", missing3.1$title)
missing3.1$title <- gsub("Tyler Perry s A Madea Chris", "A Madea Christmas", missing3.1$title)
missing3.1$title <- gsub("Demi Soeur", "Half-Sister", missing3.1$title)
missing3.1$title <- gsub("Tyler Perry s The Single Mo", "The Single Moms Club", missing3.1$title)
missing3.1$title <- gsub("Nymphomaniac Volume I", "Nymphomaniac: Vol. I", missing3.1$title)
missing3.1$title <- gsub("Nymphomaniac Volume II", "Nymphomaniac: Vol. II", missing3.1$title)
missing3.1$title <- gsub("Next Year Jerusalem", "Next Year in Jerusalem", missing3.1$title)
missing3.1$title <- gsub("James Cameron s Deepsea C", "Deepsea Challenge", missing3.1$title)
missing3.1$title <- gsub("Atlas Shrugged Who Is John", "Atlas Shrugged Part III: Who is John Galt?", missing3.1$title)
missing3.1$title <- gsub("Men Women and Children", "Men, Women & Children", missing3.1$title)
missing3.1$title <- gsub("Birdman or The Unexpected", "Birdman", missing3.1$title)
missing3.1$title <- gsub("Kirk Cameron s Saving Chris", "Saving Christmas", missing3.1$title)
missing3.1$title <- gsub("The Avengers Age of Ultron", "Avengers: Age of Ultron", missing3.1$title)
missing3.1$title <- gsub("Beyond the Brick A LEGO Br", "A LEGO Brickumentary", missing3.1$title)
missing3.1$title <- gsub("Kahlil Gibran s The Prophet", "The Prophet", missing3.1$title)
missing3.1$title <- gsub("The Man From U N C L E", "The Man from U.N.C.L.E.", missing3.1$title)
missing3.1$title <- gsub("Attack on Titan Part 1", "Attack on Titan", missing3.1$title)
missing3.1$title <- gsub("Attack on Titan Part 2", "Attack on Titan II: End of the World", missing3.1$title)
missing3.1$title <- gsub("Jafar Panahi s Taxi", "Taxi", missing3.1$title)
missing3.1$title <- gsub("Star Wars Ep VII The Forc", "Star Wars: The Force Awakens", missing3.1$title)
missing3.1$title <- gsub("The Brothers Grimsby", "Grimsby", missing3.1$title)
missing3.1$title <- gsub("Kapoor Sons", "Kapoor and Sons", missing3.1$title)
missing3.1$title <- gsub("The Divergent Series Alleg", "Allegiant", missing3.1$title)
missing3.1$title <- gsub("Ratchet and Clank", "Ratchet & Clank", missing3.1$title)
missing3.1$title <- gsub("The Conjuring 2 The Enfiel", "The Conjuring 2", missing3.1$title)
missing3.1$title <- gsub("Final Fantasy XV Kingsglaive", "Kingsglaive: Final Fantasy XV", missing3.1$title)
missing3.1$title <- gsub("Naam Hai Akira", "Akira", missing3.1$title)
missing3.1$title <- gsub("Tyler Perry s Boo A Made", "Boo 2! A Madea Halloween", missing3.1$title)
missing3.1$title <- gsub("Chris and Don A Love Story", "Chris & Don: A Love Story", missing3.1$title)
missing3.1$title <- gsub("America Imagine a World Wi", "America: Imagine the World Without Her", missing3.1$title)



w_id <- left_join(missing3.1, missing3_info, by = "title")

# update the list of movies that need additional info using id
need_info3 <- w_id %>% filter(is.na(id) == FALSE) %>% 
  dplyr::select(-c(original_title, release_date)) %>% 
  bind_rows(need_info3)

# two pages are created on TMdb website for the movie "War of the Buttons", need to choose the correct one manually
# update need_info3 list to reflect changes
need_info3 <- need_info3 %>% filter(id != 74944)

# update the list of movies that are still missing
missing3.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
dplyr::select(-c(original_title, release_date, id))

# some movies are found in the Kaggle dataset
missing3.1$title <- gsub("Son of Rambow A Home Movie", "Son of Rambow", missing3.1$title)
missing3.1$title <- gsub("Hellboy 2 The Golden Army", "Hellboy II: The Golden Army", missing3.1$title)
missing3.1$title <- gsub("The Pineapple Express", "Pineapple Express", missing3.1$title)
missing3.1$title <- gsub("Jonas Brothers The 3D Conc", "Jonas Brothers: The Concert Experience", missing3.1$title)
missing3.1$title <- gsub("Rosencrantz and Guildenstei", "Rosencrantz and Guildenstern Are Undead", missing3.1$title)
missing3.1$title <- gsub("The Twilight Saga Twilight", "Twilight", missing3.1$title)
missing3.1$title <- gsub("Hemingway s Garden of Eden", "The Garden of Eden", missing3.1$title)
missing3.1$title <- gsub("Sahib Biwi Aur Gangster", "Saheb Biwi Aur Gangster", missing3.1$title)
missing3.1$title <- gsub("If A Tree Falls The Story", "If a Tree Falls: A Story of the Earth Liberation Front", missing3.1$title)
missing3.1$title <- gsub("Nitro Circus The Movie 3D", "Nitro Circus: The Movie", missing3.1$title)
missing3.1$title <- gsub("Battle of the Year 3D", "Battle of the Year", missing3.1$title)
missing3.1$title <- gsub("The Divergent Serires Insu", "Insurgent", missing3.1$title)
missing3.1$title <- gsub("P K", "PK", missing3.1$title)
missing3.1$title <- gsub("Under the Sun 2016", "Under the Sun", missing3.1$title)
missing3.1$title <- gsub("The Queen of Katwe", "Queen of Katwe", missing3.1$title)

w_id <- left_join(missing3.1, f_movie, by = "title")

# update the list of movies that have been found
found2 <- w_id %>% filter(is.na(id) == FALSE) %>% 
  dplyr::select(-c(production_companies))
found2$id <- as.integer(found2$id)
found <- found2 %>% bind_rows(found)

# update the list of movies that are still missing
missing3.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(-c(genres, id, release_date, vote_average, production_companies, imdb_id))

# some movies can be found in its original title
w_id <- regex_right_join(missing3_info, missing3.1, by = c(original_title = "title"), ignore_case = TRUE)

# update the list of movies that need additional info
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE) %>% 
  dplyr::select(c(title.x, id, distributor, release, date, total_gross, days))
names(need_info3.1)[1] <- "title"

need_info3 <- need_info3.1 %>% bind_rows(need_info3)

# update the list of missing movies
missing3.1 <- w_id %>% filter(is.na(id) == TRUE) %>% 
  dplyr::select(c(title.y, distributor, release, date, total_gross, days))
names(missing3.1)[1] <- "title"

# some of the values have to be matched manually
one <- missing3_info[str_which(missing3_info$title,"What Happens in Vegas"),]
w_id <- bind_rows(one, missing3.1)
w_id[8,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- missing3_info[str_which(missing3_info$title,"How Do You Know"),]
w_id <- bind_rows(one, missing3.1)
w_id[38,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- missing3_info[str_which(missing3_info$title,"Luv Shuv Tey Chicken Khurana"),]
w_id <- bind_rows(one, missing3.1)
w_id[58,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- missing3_info[str_which(missing3_info$title,"10,000 BC"),]
w_id <- bind_rows(one, missing3.1)
w_id[6,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- missing3_info[str_which(missing3_info$title,"The Boondock Saints II: All Saints Day"),]
w_id <- bind_rows(one, missing3.1)
w_id[19,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- missing3_info[str_which(missing3_info$title,"Prince of Persia: The Sands of Time"),]
w_id <- bind_rows(one, missing3.1)
w_id[27,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- missing3_info[str_which(missing3_info$title,"If a Tree Falls: A Story of the Earth Liberation Front"),]
w_id <- bind_rows(one, missing3.1)
w_id[45,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- missing3_info[str_which(missing3_info$title,"Birdman"),]
w_id <- bind_rows(one, missing3.1)
w_id[60,2] <- w_id[1,2]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- f_movie[str_which(f_movie$title,"The Man from U.N.C.L.E."),]
names(one)[1] <- "title.x"
w_id <- bind_rows(one, missing3.1)
w_id[68,3] <- w_id[1,3]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- f_movie[str_which(f_movie$title,"PK"),]
names(one)[1] <- "title.x"
w_id <- bind_rows(one, missing3.1)
w_id[62,3] <- w_id[1,3]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

one <- f_movie[str_which(f_movie$title,"Under the Sun"),]
names(one)[1] <- "title.x"
w_id <- bind_rows(one, missing3.1)
w_id[72,3] <- w_id[1,3]
w_id <- w_id %>% dplyr::select(title, id, release, distributor, date, total_gross, days) %>% 
  filter(is.na(release) == FALSE)
need_info3.1 <- w_id %>% filter(is.na(id) == FALSE)
need_info3.1$id <- as.integer(need_info3.1$id)
need_info3 <- need_info3 %>% bind_rows(need_info3.1)
missing3.1 <- w_id %>% filter(is.na(id) == TRUE)

rm(one)

# the movie "ABCD2" is under another name "Any Body Can Dance 2" in TMdb database
# extract information again from TMdb using the correct movie title
m_info <- data.frame()
m_name <- "Any Body Can Dance 2"
m_name <- gsub(" ","+", m_name)
get_api(m_name)

w_id <- m_info %>% dplyr::select(c(id, title))
names(w_id)[2] <- "title.x"
w_id <- bind_rows(w_id, missing3.1)
w_id[64,3] <- w_id[1,2]
w_id[64,1] <- w_id[1,1]
need_info3 <- w_id %>% filter(is.na(release) == FALSE) %>% 
  dplyr::select(-(title.x)) %>% 
  filter(is.na(id) == FALSE) %>% 
  bind_rows(need_info3)

missing3.1 <- w_id %>% filter(is.na(release) == FALSE) %>% 
  filter(is.na(id) == TRUE) %>% 
  dplyr::select(-c(title.x, id))

# previously we have found the movie "Spices of Liberty" so we remove them from the missing list
missing3.1 <- missing3.1 %>% filter(title != "Spices of Liberty")
names(missing3.1)[1] <- "title_o"

# I modified punctuations in movie titles before and I want to revert these changes
missing3.1 <- inner_join(missing3.1,missing3, by = c("distributor" = "distributor", 
                                                     "total_gross" = "total_gross", 
                                                     "release" = "release", 
                                                     "date" = "date",
                                                     "days" = "days")) %>% 
  dplyr::select(title, distributor, release, date, total_gross, days)

found <- found %>% bind_rows(missing3.1) %>% distinct() %>% 
  filter((title != "Twilight") | (days != 1))

rm(missing_info_2)
rm(missing3)
rm(missing3_info)
rm(missing3.1)
rm(w_id)

#####################
# use ids to extract additional information for movies in the list need_info
# break down the long list into shorter lists to avoid download limits
id_list <- need_info$id

for (i in 1:(length(id_list) %/% 40)) {
  a <- (i-1)*40+1
  b <- i*40
  assign(paste('id_list',i,sep='_'), id_list[a:b])
}

assign(paste('id_list', length(id_list) %/% 40 + 1, sep = "_"), id_list[(length(id_list) %/% 40 *40 + 1):length(id_list)])

# need to manually execute these codes to avoid download rate limiting
get_detail(id_list_1)
get_detail(id_list_2)
get_detail(id_list_3)
get_detail(id_list_4)
get_detail(id_list_5)
get_detail(id_list_6)
get_detail(id_list_7)
get_detail(id_list_8)
get_detail(id_list_9)

detail_1 <- detail

# match info from TMdb database with box office data
match <- inner_join(need_info, detail_1)

rm(need_info, detail_1)

# use ids to extract additional information for movies in the list need_info2
# break down the long list into shorter lists to avoid download limits
id_list <- need_info2$id

detail <- data.frame()
get_detail(id_list)

detail_2 <- detail

match <- inner_join(need_info2, detail_2) %>% bind_rows(match)

rm(need_info2, detail_2)

# use ids to extract additional information for movies in the list need_info3
# break down the long list into shorter lists to avoid download limits
id_list <- need_info3$id

for (i in 1:(length(id_list) %/% 40)) {
  a <- (i-1)*40+1
  b <- i*40
  assign(paste('id_list',i,sep='_'), id_list[a:b])
}

assign(paste('id_list', length(id_list) %/% 40 + 1, sep = "_"), id_list[(length(id_list) %/% 40 *40 + 1):length(id_list)])

detail <- data.frame()
get_detail(id_list_1)
get_detail(id_list_2)
get_detail(id_list_3)
get_detail(id_list_4)
get_detail(id_list_5)
get_detail(id_list_6)
get_detail(id_list_7)
get_detail(id_list_8)
get_detail(id_list_9)
get_detail(id_list_10)
get_detail(id_list_11)
get_detail(id_list_12)
get_detail(id_list_13)
get_detail(id_list_14)

detail_3 <- detail

match <- inner_join(need_info3, detail_3) %>% bind_rows(match)

rm(need_info3, detail_3)

####################

# f_movie <- f_movie %>% tidyr::separate(production_companies, c("p1", "p2", "p3", "p4", "p5", 'p6','p7','p8',
#                                                                 'p9','p10','p11','p12','p13','p14','p15','p16',
#                                                                 'p17','p18','p19','p20','p21','p22','p23','p24',
#                                                                 'p25','p26'), sep = "\\}, ", fill = "right")


# f_movie$genres <- gsub("'name':", "", f_movie$genres)
# f_movie$genres <- gsub("'id':", "", f_movie$genres)
# f_movie$genres <- gsub("[[:digit:]]+", "", f_movie$genres)
# f_movie$genres <- gsub("'", "", f_movie$genres)
# f_movie$genres <- gsub("\\[|\\]", "", f_movie$genres)
# f_movie$genres <- gsub(" ", "", f_movie$genres)
# f_movie$genres <- gsub("\\{,", "", f_movie$genres)

# separate the "genre" column for Kaggle dataset so that each column only contains one genre
# f_movie <- f_movie %>% tidyr::separate(genres, c("g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8"), sep="\\}",
#                                         extra = "drop", fill = "right")


###########################
save.image('envr.RData')

save.image("04_21.RData")

load("./envr.RData")

load("04_21.RData") # load this file first  (save data)