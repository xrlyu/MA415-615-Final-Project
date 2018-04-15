## now work with API to dowonload details of movies
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("lubridate")

library(httr)
library(jsonlite)
library(lubridate)

# transform the list of movies
# movie_list <- total %>% select(movie, release) %>% distinct() # a list of movies released in the past decade with release date
# m_list <- sapply(movie[,1],function(x) gsub("[[:punct:] ]+"," ", x))
# m_list <- sapply(m_list, function(x) gsub(" ","+", x)) # for API

# write a function to download data from TMdb website
m_info <- data.frame()
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
  m_info <<- rbind(m_info, m_total)
}


# get data
# have to manually do this because of request rate limiting
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
# get_api(m_list_17)
# get_api(m_list_18)
# get_api(m_list_19)
# get_api(m_list_20)
# get_api(m_list_21)
# get_api(m_list_22)
# get_api(m_list_23)
# get_api(m_list_24)
# get_api(m_list_25)
# get_api(m_list_26)
# get_api(m_list_27)
# get_api(m_list_28)
# get_api(m_list_29)
# get_api(m_list_30)
# get_api(m_list_31)
# get_api(m_list_32)
# get_api(m_list_33)
# get_api(m_list_34)
# get_api(m_list_35)
# get_api(m_list_36)
# get_api(m_list_37)
# get_api(m_list_38)
# get_api(m_list_39)
# get_api(m_list_40)
# get_api(m_list_41)
# get_api(m_list_42)
# get_api(m_list_43)
# get_api(m_list_44)
# get_api(m_list_45)
# get_api(m_list_46)
# get_api(m_list_47)
# get_api(m_list_48)
# get_api(m_list_49)
# get_api(m_list_50)
# get_api(m_list_51)
# get_api(m_list_52)
# get_api(m_list_53)
# get_api(m_list_54)
# get_api(m_list_55)
# get_api(m_list_56)
# get_api(m_list_57)
# get_api(m_list_58)
# get_api(m_list_59)
# get_api(m_list_60)
# get_api(m_list_61)
# get_api(m_list_62)
# get_api(m_list_63)
# get_api(m_list_64)
# get_api(m_list_65)
# get_api(m_list_66)
# get_api(m_list_67)
# get_api(m_list_68)
# get_api(m_list_69)
# get_api(m_list_70)
# get_api(m_list_71)
# get_api(m_list_72)
# get_api(m_list_73)
# get_api(m_list_74)
# get_api(m_list_75)
# get_api(m_list_76)
# get_api(m_list_77)
# get_api(m_list_78)
# get_api(m_list_79)
# get_api(m_list_80)

# save movie information to RData file for cleaning

# save(m_info, file = "./Data/movie_info.RData")


## api
## 2c2d3eeeb39c7d6d18608fbdcd6407ad