## now work with API to dowonload details of movies
# install.packages("httr")
# install.packages("jsonlite")

library(httr)
library(jsonlite)

# write a function to download data from TMdb website
m_info <- data.frame()
get_api <- function(movie_list) {
  m_total <- data.frame()
  for (i in 1:length(movie_list)) {
    dataset <- GET(paste("https://api.themoviedb.org/3/search/movie?api_key=2c2d3eeeb39c7d6d18608fbdcd6407ad&query=", movie_list[i], sep=""))
    this.raw.content <- rawToChar(dataset$content)
    this.content <- fromJSON(this.raw.content)
    content <- this.content$results
    m_total <- rbind(m_total,content)
  }
  m_total <<- m_total
  m_info <<- rbind(m_info, m_total)
}

# write a function to query details of movies from TMdb website
detail <- data.frame()
get_detail <- function(id_list) {
  d_info <- data.frame()
  for (i in 1:length(id_list)) {
    dataset <- GET(paste("https://api.themoviedb.org/3/movie/",id_list[i],"?api_key=2c2d3eeeb39c7d6d18608fbdcd6407ad", sep = ""))
    this.raw.content <- rawToChar(dataset$content)
    this.content <- fromJSON(this.raw.content)
    data <- this.content[c(5, 7, 8, 22, 24, 25)] # only need information on genre, id, imdb_id, title, vote_average, vote_count
    na_frame <- cbind(NA,NA)
    data$genres <- if (is.null(dim(data$genres)) == TRUE) {
      data$genres <- na_frame} else{
        data$genres <- data$genres
      }
    data$genres <- data$genres[,2]
    data$imdb_id <- ifelse(is.null(data$imdb_id), NA, data$imdb_id)
    data$vote_average <- ifelse(is.null(data$vote_average), NA, data$vote_average)
    data$vote_count <- ifelse(is.null(data$vote_count), NA, data$vote_count)
    data <- as.data.frame(data)
    d_info <- rbind(d_info, data)
  }
  d_info <<- d_info
  detail <<- rbind(detail, d_info) 
}


## api
## 2c2d3eeeb39c7d6d18608fbdcd6407ad