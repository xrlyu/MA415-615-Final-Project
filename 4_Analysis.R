library(dplyr)
library(tidyverse)
library(forcats)
# install.packages("ggrepel")
library(ggrepel)

data <- readRDS("./Data/tidy.rds") # load tidy data

data$gross <- as.numeric(data$gross)

data <- data %>% dplyr::select(-distributor) %>% distinct()

data2 <- data %>% dplyr::select(-c(genres)) %>% distinct() %>% 
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

oscars$gross <- as.numeric(oscars$gross)

oscars <- inner_join(oscars, final)

oscars$year <- factor(oscars$year)

oscars$month <- fct_inorder(as.factor(oscars$month))

### most recent week data
most_recent <- readRDS("./Data/tidy_current.rds")

most_recent$gross <- as.numeric(most_recent$gross)

most_recent <- inner_join(most_recent, final)

most_recent$year <- factor(most_recent$year)

most_recent$month <- factor(most_recent$month)

levels(most_recent$month) <- c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec")


### 

final$title <- factor(final$title, levels = unique(final[order(final$year, decreasing = T),]$title))

final$year <- factor(final$year)

final$month <- fct_inorder(factor(final$month))
  
### Summary for the full dataset

# Number of movies release based on genres

plot_1 <- ggplot(data = final %>% dplyr::select(-genres) %>% distinct(), aes(x = year)) +
  geom_bar(aes(fill = month), position = "dodge") +
  labs(x = "", y = "Number of Movies Released")

plot_2 <- ggplot(data = final, aes(x = fct_infreq(final$genres))) +
  geom_bar(aes(fill = year)) +
  geom_text(stat='count', aes(label=..count..), size = 3, hjust = -0.2) +
  coord_flip() +
  labs(x = "Genres", y ="Number of Movies")

plot_3 <- ggplot(data = final, aes(x = fct_infreq(final$genres))) +
  geom_bar(aes(fill = month)) +
  geom_text(stat = 'count', aes(label = ..count..), size = 3, hjust= -0.2) +
  coord_flip() +
  labs(x= "Genres", y = "Number of Movies")
  
plot_4 <- ggplot(data = final %>% dplyr::select(-genres) %>% distinct(), aes(color = Year)) +
  geom_jitter(aes(x = vote_average, y = gross, color = year), alpha = 2/5, size = 3) +
  labs(x = "Voting Score", y = "Gross Box office (in Dollar)") +
  geom_smooth(aes(x = vote_average, y = gross), method = "lm", color = "salmon", size = 0.5)

plot_5 <- ggplot(data = final) +
  geom_boxplot(aes(x = genres, y = gross, color = genres)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
  labs(x = "Genres", y = "Gross Box Office (in Dollar)")

plot_6 <- ggplot(data = final) +
  geom_boxplot(aes(x = genres, y = vote_average, color = genres)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
  labs(x = "Genres", y = "Voting Score")

cor(final$vote_average, final$gross, use = "complete")

### Movies of the highest gross box office every year

h_1 <- final %>% group_by(year) %>% filter(gross == max(gross)) %>% ungroup()

h_1$genres <- fct_infreq(h_1$genres)

h_1_genre <- ggplot(data = h_1, aes(x = genres)) +
  geom_bar(aes(fill = year)) +
  labs(x = "Genres", y ="Number of Movies", legend = "Movie Titles") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

h_1_box_genre <- ggplot(data = h_1, aes(x = genres)) +
  geom_path(data = h_1 %>% group_by(genres) %>% summarize(mean(gross)),
            aes(y = `mean(gross)`, group = 1), color = "navyblue") + 
  labs(x = "Genres", y = "Gross Box Office (in Dollar)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

h_1_vote_genre <- ggplot(data = h_1 %>% group_by(genres) %>% summarize(mean(vote_average))) +
  geom_path(aes(x= genres, y = `mean(vote_average)`, group = 1), color ="navyblue") +
  labs(x = "Genres", y = "Voting Score") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

h_1_box_vote <- ggplot(data = h_1 %>% dplyr::select(-genres) %>% distinct()) +
  geom_point(aes(x = vote_average, y = gross), color = "navyblue", shape = 15) +
  geom_text(aes(x = vote_average, y = gross, label = year), color = "cornflowerblue", size = 4,
            vjust =-0.7) +
  geom_smooth(aes(x = vote_average, y = gross), method = "lm", color = 'steelblue1', group = 1) +
  labs(x = "Voting Score", y = "Gross Box Office (in Dollar)")
  
h_1_rate <- ggplot(data = h_1 %>% dplyr::select(-genres) %>% distinct() %>% arrange(year)) +
  geom_line(mapping = aes(x= title, y = vote_average, colour = "Actual", group = 1), size = 1.5) +
  geom_line(mapping = aes(x = title, y = avg_y_rate, color = "Yearly Average", group = 2)) +
  geom_line(mapping = aes(x = title, y = avg_m_rate, color = "Monthly Average", group = 3)) +
  geom_line(mapping = aes(x = title, y = avg_alm_rate, color = "Monthly Average \nof All Years", group = 4)) +
  annotate("text", x = 1:11, y = 8.25, label = unique(sort(h_1$year, decreasing = TRUE)), size = 3, color = "grey74", angle = 90) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_smooth(mapping = aes(x= title, y = vote_average, group = 6), method = "lm") +
  labs(x = "", y = "Voting Score", title = "Comparison of Voting Scores", color = "Legend") # comparison of voting score

h_1_box <- ggplot(data = h_1 %>% dplyr::select(-genres) %>% distinct()) +
  geom_line(mapping = aes(x= title, y = gross, colour = "Actual", group = 1), size = 1.5) +
  geom_line(mapping = aes(x = title, y = avg_y_gross, color = "Yearly Average", group = 2)) +
  geom_line(mapping = aes(x = title, y = avg_m_gross, color = "Monthly Average", group = 3)) +
  geom_line(mapping = aes(x = title, y = avg_alm_gross, color = "Monthly Average \nof All Years"), group = 4) +
  annotate("text", x = 1:11, y = 1*10^9, label = unique(sort(h_1$year, decreasing = TRUE)), size = 3, color = "grey74", angle = 90) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_smooth(mapping = aes(x= title, y = gross, group = 6), method = "lm") +
  labs(x = "", y = "Box Office (in Dollar)", title = "Comparison of Box Office", color = 'Legend') # comparison of box office


### Movies of the highest gross box office every month (of all years)

h_2 <- final %>% group_by(month) %>% filter(gross == max(gross)) %>% ungroup()

h_2$genres <- fct_infreq(h_2$genres)

h_2_rate <- ggplot(data = h_2 %>% dplyr::select(-c(genres, year)) %>% distinct()) +
  geom_line(mapping = aes(x = month, y = vote_average, color = "Actual", group = 1), size = 1.5) +
  geom_line(mapping = aes(x = month, y = avg_y_rate, color = "Yearly Average", group = 2)) +
  geom_line(mapping = aes(x = month, y = avg_m_rate, color = "Mothly Average", group = 3)) +
  geom_line(mapping = aes(x = month, y = avg_alm_rate, color = "Monthly Average \nof All Years", group = 4)) +
  geom_smooth(mapping = aes(x= month, y = vote_average, group = 6), method = "lm") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "", y = "Voting Score", color = "Legend")

h_2_box <- ggplot(data = h_2 %>% dplyr::select(-genres) %>% distinct()) +
  geom_line(mapping = aes(x= month, y = gross, color = "Actual", group = 1), size = 1.5) +
  geom_line(mapping = aes(x = month, y = avg_y_gross, color = "Yearly Average", group = 2)) +
  geom_line(mapping = aes(x = month, y = avg_m_gross, color = "Mothly Average", group = 3)) +
  geom_line(mapping = aes(x = month, y = avg_alm_gross, color = "Monthly Average \nof All Years", group = 4)) +
  geom_smooth(mapping = aes(x= month, y = gross, group = 6), method = "lm") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "", y = "Gross Box Office (in Dollar)", color = "Legend")

h_2_genre <- ggplot(data = h_2, aes(x = genres)) +
  geom_bar(aes(fill = month)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Genres", y ="Number of Movies")

h_2_box_genre <- ggplot(data = h_2, aes(x = genres)) +
  geom_path(data = h_2 %>% group_by(genres) %>% summarize(mean(gross)),
            aes(y = `mean(gross)`, group = 1), color = "navyblue") + 
  labs(x = "Genres", y = "Gross Box Office (in Dollar)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

h_2_vote_genre <- ggplot(data = h_2 %>% group_by(genres) %>% summarize(mean(vote_average))) +
  geom_path(aes(x = genres, y = `mean(vote_average)`, group = 1), color ="navyblue") +
  labs(x = "Genres", y = "Voting Score") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

h_2_box_vote <- ggplot(data = h_2 %>% dplyr::select(-genres) %>% distinct()) +
  geom_point(aes(x = vote_average, y = gross), color = "navyblue", shape = 15) +
  geom_text(aes(x = vote_average, y = gross, label = year), color = "cornflowerblue", size = 4,
            vjust =-0.7) +
  geom_text(aes(x = vote_average, y = gross, label = month), color = "red", size = 4,
            vjust = 1.2) +
  geom_smooth(aes(x = vote_average, y = gross), method = "lm", color = "steelblue1", group = 1) +
  labs(x = "Voting Score", y = "Gross Box Office (in Dollar)")

### Oscars Best Pictures

oscars_time <- ggplot(data = oscars %>% dplyr::select(-genres) %>% distinct()) +
  geom_bar(aes(x = month, fill = year)) + 
  labs (x = "", y = "Number of Movies")

oscars_genre <- ggplot(data = oscars) +
  geom_bar(aes(x = genres, fill = year)) +
  labs(x = "Genres", y = "Number of Movies" )

oscars_vote <- ggplot() +
  geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
             mapping = aes(x= year, y = vote_average, colour = "Oscars", group = 1), size = 1.5) +
  geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
             mapping = aes(x = year, y = avg_y_rate, color = "Yearly Average", group = 2)) +
  geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
             mapping = aes(x = year, y = avg_m_rate, color = "Monthly Average", group = 3)) +
  geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
             mapping = aes(x = year, y = avg_alm_rate, color = "Monthly Average \nof All Years",
                           group = 4)) +
  geom_line(data = h_1 %>% dplyr::select(-genres) %>% distinct() %>% filter(year != 2018),
             mapping = aes(x = year, y = vote_average, color = "Yearly Highest", group = 5)) + 
  geom_smooth(data = oscars %>% dplyr::select(-genres) %>% distinct(),
              mapping = aes(x= year, y = vote_average, group = 6), method = "lm") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "", y = "Voting Score", color = 'Legend') # comparison of voting score

oscars_box <- ggplot() +
  geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
             mapping = aes(x= year, y = gross, colour = "Oscars", group = 1), size = 1.5) +
  geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
             mapping = aes(x = year, y = avg_y_gross, color = "Yearly Average", group = 2)) +
  geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
             mapping = aes(x = year, y = avg_m_gross, color = "Monthly Average", group = 3)) +
  geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
             mapping = aes(x = year, y = avg_alm_gross, color = "Monthly Average \nof All Years"), group = 4) +
  geom_line(data = h_1 %>% dplyr::select(-genres) %>% distinct() %>% filter(year != 2018),
            mapping = aes(x = year, y = gross, color = "Yearly Highest", group = 5)) + 
  geom_smooth(data = oscars %>% dplyr::select(-genres) %>% distinct(),
              mapping = aes(x= year, y = gross, group = 6), method = "lm") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "", y = "Gross Box Office", color = 'Legend') # comparison of box office

oscars_box_vote <- ggplot(data = oscars %>% dplyr::select(-genres) %>% distinct) +
  geom_point(aes(x = vote_average, y = gross), color = "navyblue", shape = 15) +
  geom_text(aes(x = vote_average, y = gross, label = year), color = "cornflowerblue", size = 4,
            vjust = -0.7) +
  labs(x = "Voting Score", y = "Gross Box Office") +
  geom_smooth(aes(x = vote_average, y = gross), method = "lm", color = 'steelblue1')
  

cor(oscars$vote_average,oscars$gross)

###### Movies released in April

most_recent <- arrange(most_recent, title)

recent_time <- ggplot(data = most_recent %>% dplyr::select(-genres) %>% distinct()) +
  geom_bar(aes(x = month, fill = title)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "bottom", legend.text = element_text(size = 7)) +
  labs (x = "", y = "Number of Movies", fill = "Movie Titles")


recent_genre <- ggplot(most_recent) +
  geom_bar(aes(x = genres, fill = title)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "bottom", legend.text = element_text(size = 7)) +
  labs(x = "Genres", y = "Number of Movies", fill = "Movie Titles")

recent_oscars <- ggplot() +
  labs(x = "Voting Score", y = "Gross Box Office (in Dollar)") +
  geom_label_repel(data = most_recent %>% dplyr::select(-genres) %>% distinct,
             aes(x = vote_average, y = gross, label = title), size = 3) +
  geom_smooth(data = oscars %>% dplyr::select(-genres) %>% distinct,
              aes(x = vote_average, y = gross), method = "lm", color = 'steelblue1', fullrange = TRUE)


