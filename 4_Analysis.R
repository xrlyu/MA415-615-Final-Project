library(dplyr)
library(tidyverse)
library(forcats)

data <- readRDS("./Data/tidy.rds") # load tidy data

data$gross <- as.numeric(data$gross)

data2 <- data %>% dplyr::select(-genres) %>% distinct() %>% 
  dplyr::group_by(year) %>% 
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

### Oscars Best Picture

oscars <- readRDS("./Data/bestpic.rds")

oscars <- inner_join(oscars, final)

oscars$year <- factor(oscars$year)

oscars$month <- fct_inorder(as.factor(oscars$month))

### 

final$title <- factor(final$title, levels = unique(final[order(final$year, decreasing = T),]$title))

final$year <- factor(final$year)

final$month <- fct_inorder(factor(final$month))
  
### Summary for the full dataset

# Number of movies release based on genres

plot_1 <- ggplot(data = final %>% dplyr::select(-genres) %>% distinct(), aes(x = year)) +
  geom_bar(aes(fill = month), position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Number of Movies Released")

plot_2 <- ggplot(data = final, aes(x = fct_infreq(final$genres))) +
  geom_bar(aes(fill = year)) +
  geom_text(stat='count', aes(label=..count..), size = 3, hjust = -0.2) +
  coord_flip() +
  labs(x = "Genres", y ="Number of Movies", title = "Number of Movies Released by Genre",
       subtitle= "Yearly Data")

plot_3 <- ggplot(data = final, aes(x = fct_infreq(final$genres))) +
  geom_bar(aes(fill = month)) +
  geom_text(stat = 'count', aes(label = ..count..), size = 3, hjust= -0.2) +
  coord_flip() +
  labs(x= "Genres", y = "Number of Movies", title = "Number of Movies Released by Genre",
       subtitle = "Monthly Data")
  
plot_4 <- ggplot(data = final %>% dplyr::select(-genres) %>% distinct(), aes(color = Year)) +
  geom_jitter(aes(x = vote_average, y = gross, color = year), alpha = 2/5, size = 3) +
  labs(x = "Voting Score", y = "Gross Box office") +
  geom_smooth(aes(x = jitter(vote_average), y = jitter(gross)), color = "salmon", size = 0.5)

cor(final$vote_average, final$gross, use = "complete")

### Movies of the highest gross box office every year

h_1 <- final %>% group_by(year) %>% filter(gross == max(gross)) 

h_1$genres <- fct_infreq(h_1$genres)

h_1_genre <- ggplot(data = h_1, aes(x = genres)) +
  geom_bar() +
  coord_flip() +
  labs(x = "Genres", y ="Count")
  
h_1_rate <- ggplot(data = h_1 %>% dplyr::select(-genres) %>% distinct(), aes(color = legend)) +
  geom_point(mapping = aes(x= title, y = vote_average, colour = "Actual")) +
  geom_point(mapping = aes(x = title, y = avg_y_rate, color = "Yearly Average")) +
  geom_point(mapping = aes(x = title, y = avg_m_rate, color = "Monthly Average")) +
  geom_point(mapping = aes(x = title, y = avg_alm_rate, color = "Monthly Average \nof All Years")) +
  annotate("text", x = 1:11, y = 8.7, label = unique(sort(h_1$year, decreasing = TRUE)), size = 3, color = "grey74") +
  coord_flip() +
  labs(x = "", y = "Voting Score", title = "Each Year's Movie with Highest Gross Box Office",
       subtitle = "A Comparison of Voting Score") # comparison of voting score

h_1_box <- ggplot(data = h_1 %>% dplyr::select(-genres) %>% distinct(), aes(color = legend)) +
  geom_point(mapping = aes(x= title, y = gross, colour = "Actual")) +
  geom_point(mapping = aes(x = title, y = avg_y_gross, color = "Yearly Average")) +
  geom_point(mapping = aes(x = title, y = avg_m_gross, color = "Monthly Average")) +
  geom_point(mapping = aes(x = title, y = avg_alm_gross, color = "Monthly Average \nof All Years")) +
  annotate("text", x = 1:11, y = 1.15*10^9, label = unique(sort(h_1$year, decreasing = TRUE)), size = 3, color = "grey74") +
  coord_flip() +
  labs(x = "", y = "Box Office", title = "Each Year's Movie with Highest Gross Box Office",
       subtitle = "A Comparison of Box Office Numbers") # comparison of box office


### Movies of the highest gross box office every month (of all years)

h_2 <- final %>% group_by(month) %>% filter(gross == max(gross)) %>% arrange(month)

ggplot(data = h_2 %>% dplyr::select(-genres) %>% distinct() %>% arrange(year)) +
  geom_point(mapping = aes(x= title, y = vote_average, color = "Actual")) +
  geom_point(mapping = aes(x = title, y = avg_y_rate, color = "Yearly Average")) +
  geom_point(mapping = aes(x = title, y = avg_m_rate, color = "Mothly Average")) +
  geom_point(mapping = aes(x = title, y = avg_alm_rate, color = "Monthly Average \nof All Years")) +
  coord_flip() +
  labs(x = "", y = "Box Office")

### Movies of the highest gross box office every month (every year)

h_3 <- final %>% group_by(year, month) %>% filter(gross == max(gross))

# ggplot(data = h_3 %>% dplyr::select(-genres) %>% distinct() %>% arrange(year)) +
#   geom_point(mapping = aes(x= title, y = vote_average, color = "Actual")) +
#   geom_point(mapping = aes(x = title, y = avg_y_rate, color = "Yearly Average")) +
#   geom_point(mapping = aes(x = title, y = avg_m_rate, color = "Mothly Average")) +
#   geom_point(mapping = aes(x = title, y = avg_alm_rate, color = "Monthly Average \nof All Years")) +
#   coord_flip() +
#   labs(x = "", y = "Voting Score")

### Movies of the lowest gross box office every year

l_1 <- final %>% group_by(year) %>% filter(gross == min(gross))

ggplot(data = l_1 %>% dplyr::select(-genres) %>% distinct() %>% arrange(year)) +
  geom_point(mapping = aes(x= title, y = vote_average, color = "Actual")) +
  geom_point(mapping = aes(x = title, y = avg_y_rate, color = "Yearly Average")) +
  geom_point(mapping = aes(x = title, y = avg_m_rate, color = "Mothly Average")) +
  geom_point(mapping = aes(x = title, y = avg_alm_rate, color = "Monthly Average \nof All Years")) +
  coord_flip() +
  labs(x = "", y = "Voting Score")

### Movies of the lowest gross box office every month (of all years)

l_2 <- final %>% group_by(month) %>% filter(gross == min(gross))

ggplot(data = l_2 %>% dplyr::select(-genres) %>% distinct() %>% arrange(year)) +
  geom_point(mapping = aes(x= title, y = vote_average, color = "Actual")) +
  geom_point(mapping = aes(x = title, y = avg_y_rate, color = "Yearly Average")) +
  geom_point(mapping = aes(x = title, y = avg_m_rate, color = "Mothly Average")) +
  geom_point(mapping = aes(x = title, y = avg_alm_rate, color = "Monthly Average \nof All Years")) +
  coord_flip() +
  labs(x = "", y = "Voting Score")

### Movies of the lowest gross box office every month (every year)

l_3 <- final %>% group_by(month, year) %>% filter(gross == min(gross))

ggplot(data = l_3 %>% dplyr::select(-genres) %>% distinct() %>% arrange(year)) +
  geom_point(mapping = aes(x= title, y = vote_average, color = "Actual")) +
  geom_point(mapping = aes(x = title, y = avg_y_rate, color = "Yearly Average")) +
  geom_point(mapping = aes(x = title, y = avg_m_rate, color = "Mothly Average")) +
  geom_point(mapping = aes(x = title, y = avg_alm_rate, color = "Monthly Average \nof All Years")) +
  coord_flip() +
  labs(x = "", y = "Voting Score")

### Oscars Best Pictures

ggplot(data = oscars %>% dplyr::select(-genres) %>% distinct()) +
  geom_bar(aes(x = month)) + 
  labs (x = "")

ggplot(data = oscars) +
  geom_bar(aes(x = genres, fill = year))

ggplot(data = oscars %>% dplyr::select(-genres) %>% distinct(), aes(color = legend)) +
  geom_point(mapping = aes(x= title, y = vote_average, colour = "Oscars")) +
  geom_point(mapping = aes(x = title, y = avg_y_rate, color = "Yearly Average")) +
  geom_point(mapping = aes(x = title, y = avg_m_rate, color = "Monthly Average")) +
  geom_point(mapping = aes(x = title, y = avg_alm_rate, color = "Monthly Average \nof All Years")) +
  annotate("text", x = 1:10, y = 8.7, label = unique(sort(oscars$year, decreasing = TRUE)), size = 3, color = "grey74") +
  coord_flip() +
  labs(x = "", y = "Voting Score", title = "Each Year's Movie with Highest Gross Box Office",
       subtitle = "A Comparison of Voting Score") # comparison of voting score

ggplot(data = oscars %>% dplyr::select(-genres) %>% distinct(), aes(color = legend)) +
  geom_point(mapping = aes(x= title, y = gross, colour = "Actual")) +
  geom_point(mapping = aes(x = title, y = avg_y_gross, color = "Yearly Average")) +
  geom_point(mapping = aes(x = title, y = avg_m_gross, color = "Monthly Average")) +
  geom_point(mapping = aes(x = title, y = avg_alm_gross, color = "Monthly Average \nof All Years")) +
  annotate("text", x = 1:10, y = 1.15*10^9, label = unique(sort(oscars$year, decreasing = TRUE)), size = 3, color = "grey74") +
  coord_flip() +
  labs(x = "", y = "Box Office", title = "Each Year's Movie with Highest Gross Box Office",
       subtitle = "A Comparison of Box Office Numbers") # comparison of box office

cor(oscars$vote_average,oscars$gross)
