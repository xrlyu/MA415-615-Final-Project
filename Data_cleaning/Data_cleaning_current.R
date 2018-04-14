library(dplyr)
library(tidyverse)

# read data
current <- readRDS("./Data/current.rds")

current_tidy <- current %>% 
  mutate(release = date - (days-1)) %>% 
  dplyr::select(c(movie, distributor, release, date, total_gross, days)) %>% 
  group_by(movie) %>% 
  top_n(1, days) %>%
  mutate(avg.gross = total_gross/days)

current_shiny <-  current %>% 
  mutate(release = date - (days-1)) %>% 
  group_by(movie) %>% 
  top_n(1, days) %>% 
  arrange(desc(total_gross))

# save tidy data
saveRDS(current_tidy, "./Data/tidy_current.rds")
saveRDS(current_shiny, "./Data/current_shiny.rds")
