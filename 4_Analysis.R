library(dplyr)
library(tidyverse)

data <- readRDS("./Data/tidy.rds") # load tidy data

data2 <- data %>% dplyr::select(-genres) %>% distinct() %>% 
  group_by(year) %>% 
  mutate(avg_y_gross = mean(gross)) %>%  # yearly average box office
  mutate(avg_y_rate = mean(vote_average, na.rm = TRUE)) %>%  # yearly voting average
  ungroup() %>% 
  group_by(year, month) %>%
  mutate(avg_m_gross = mean(gross)) %>% # average monthly box office for every year
  mutate(avg_m_rate = mean(vote_average, na.rm = TRUE)) %>% # monthly voting average every year
  ungroup() %>% 
  group_by(month) %>% 
  mutate(avg_alm_gross = mean(gross)) %>%  # monthly box office average of all years
  mutate(avg_alm_rate = mean(vote_average, na.rm = TRUE)) # monthly voting average of all years

final <- inner_join(data, data2) # inner join two datasets

rm(data, data2)

### Movies of the highest gross box office every year

h_1 <- final %>% group_by(year) %>% filter(gross == max(gross)) 

### Movies of the highest gross box office every month (of all years)

h_2 <- final %>% group_by(month) %>% filter(gross == max(gross)) %>% arrange(month)

### Movies of the highest gross box office every month (every year)

h_3 <- final %>% group_by(year, month) %>% filter(gross == max(gross))

### Movies of the lowest gross box office every year

l_1 <- final %>% group_by(year) %>% filter(gross == min(gross))

### Movies of the lowest gross box office every month (of all years)

l_2 <- final %>% group_by(month) %>% filter(gross == min(gross))

### Movies of the lowest gross box office every month (every year)

l_3 <- final %>% group_by(month, year) %>% filter(gross == min(gross))


