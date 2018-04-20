library(dplyr)
library(tidyverse)
library(data.table)
library(stringr)

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
missing <- duplicate %>% filter(release >= as.Date("2017-01-01"))

# update the list of duplicated movies by excluding "missing" dataset
duplicate <- duplicate %>% filter(release < as.Date("2017-01-01"))

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
duplicate$diff <- difftime(duplicate$release, duplicate$release_date)

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
  dplyr::select(-c(diff))

rm(duplicate)

###########################
# list of movies that are not in Kaggle dataset and need additional information
missing3 <- w_id %>% filter(release < as.Date("2017-01-01")) %>% 
  filter(is.na(id) == TRUE)

# download movie information for movies 
# first for the list of movies in "missing"
m_list <- sapply(missing$title,function(x) gsub("[[:punct:] ]+"," ", x)) # replace punctuations with blank space
m_list <- sapply(m_list, function(x) gsub(" ","+", x)) # insert "+" between words

get_api(m_list)

missing_info <- m_info

# then for the list of movies in "missing2"
m_list <- sapply(missing2$title,function(x) gsub("[[:punct:] ]+"," ", x))
m_list <- sapply(m_list, function(x) gsub(" ","+", x))

get_api(m_list)

missing2_info <- m_info


# found <- duplicate %>% group_by(title, distributor) %>% 
#   top_n(-1, diff) %>% group_by(title) %>% filter(n_distinct(distributor)==1) %>% 
#   dplyr::select(-c(release_date, diff, date, production_companies)) %>% 
#   bind_rows(found)

# update the list of duplicated values 
duplicate <- duplicate[!(duplicate$title %in% found$title),]

missing <- duplicate %>% filter(is.na(id) == TRUE) %>% dplyr::select(-diff)
missing <- w_id %>% filter(is.na(id) == TRUE) %>% bind_rows(missing)

duplicate <- duplicate[!(duplicate$title %in% missing$title),]

w_id <- w_id %>% arrange(title, release)

save(duplicate, f_movie, found, total, w_id, missing, file = "envr.RData")
#######################
load("./envr.RData")
#######################

found1 <- w_id %>% filter(is.na(id) == FALSE) %>% filter(duplicated(title) == TRUE)
found2 <- w_id[!(w_id$title %in% found1$title),] %>% filter(is.na(id) == FALSE)

check_info <- found2 %>% filter(release >= as.Date("2017-01-01"))

a <- "Summit Entertainment"
b <- "Summit Entertainment,  }, Witt/Thomas Productions,  }, Depth of Field,  }, McLaughlin Films,  }, Lime Orchard Productions,  }"

str_detect(b,a)




need_info <- total %>% filter(release >= as.Date("2017-01-01")) %>% 
  dplyr::select(c(movie, distributor, release, date, total_gross, days)) %>% 
  group_by(movie) %>% 
  top_n(1, days)

rest <- anti_join(total, need_info)

names(need_info)[1] <- "title"
names(total)[1] <- "title"
names(rest)[1] <- "title"

need_info <- as.data.frame(need_info)
total <- as.data.frame(total)
rest <- as.data.frame(rest)

## first, work with movies before July 2017


rest_duplicate <- filter(w_id, duplicated(w_id$title) == TRUE) %>% 
  dplyr::select(c(title, distributor, release, date, total_gross, days)) %>% 
  distinct()


### work with data beofre July 2017

w_id <- left_join(total, f_movie, by = "title") %>% 
  arrange(title)

duplicate <- filter(w_id, duplicated(title) == TRUE) %>% 
  dplyr::select(title, distributor, release, total_gross, days) %>% 
  distinct()

missing <- filter(w_id, is.na(id) == TRUE) %>% 
  dplyr::select(title, distributor, release, total_gross, days)

found <- filter(w_id, duplicated(title) == FALSE) %>% 
  filter(is.na(id) == FALSE)
  
#####################
# save tidy data
saveRDS(historical_tidy, "./Data/tidy_historical.rds")


####################

# f_movie <- f_movie %>% tidyr::separate(production_companies, c("p1", "p2", "p3", "p4", "p5", 'p6','p7','p8',
#                                                                 'p9','p10','p11','p12','p13','p14','p15','p16',
#                                                                 'p17','p18','p19','p20','p21','p22','p23','p24',
#                                                                 'p25','p26'), sep = "\\}, ", fill = "right")




# # Wonderwoman wih correct release date
# found$diff <- abs(difftime(found$release, found$release_date))
# 
# found <- found %>% group_by(title, distributor) %>% 
#   top_n(-1, diff) %>% 
#   dplyr::select(-c(release_date, diff, date))
