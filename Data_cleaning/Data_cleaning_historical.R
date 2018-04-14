library(dplyr)
library(tidyverse)

# read data
historical <- readRDS("./Data/historical.rds")

# inflation rate

historical_tidy <- historical %>% 
  mutate(release = date - (days-1)) %>% 
  dplyr::select(c(movie, distributor, release, date, total_gross, days)) %>% 
  group_by(movie) %>% 
  top_n(1, days) %>% 
  mutate(avg.gross = total_gross/days)

# save tidy data
saveRDS(historical_tidy, "./Data/tidy_historical.rds")