library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)

install.packages("ggridges")
install.packages("ggExtra")

library(ggridges)
library(ggExtra)


atp_singles_data <- readRDS(file = here::here("data","processed","atp_singles.Rds")) 
head(atp_singles_data)


filtered_atp_singles <- atp_singles_data %>%
  filter(
    winner_name %in% c("Roger Federer", "Novak Djokovic", "Rafael Nadal") 
  ) %>%
  filter(round == "F") %>%
  select(tourney_name, tourney_date, winner_name, winner_rank)
filtered_atp_singles

######################################################################################################################

plot_1 <- ggplot(filtered_atp_singles, aes(x = winner_rank, y = winner_name, fill = winner_name)) +
  geom_density_ridges() +
  xlim(1,20) +
  labs(x = "Ranking", y = "", fill = "Player", title = "Density Distribution of Winner Rankings", 
       subtitle = "Big 3 Tournament Wins") +
  theme_minimal()
plot_1


######################################################################################################################


filtered_atp_singles_2 <- atp_singles_data %>%
  filter(round == "F") %>%
  filter(!is.na(winner_rank)) %>%
  group_by(winner_name) %>%
  mutate(number_titles = n()) %>%
  mutate(avg_rank = mean(winner_rank, na.rm = TRUE)) %>%
  select(winner_name, number_titles, avg_rank)
filtered_atp_singles_2

plot_2 <- ggplot(filtered_atp_singles_2, aes(x = number_titles, y = avg_rank)) +
  geom_point() +
  labs(x = "Number of Titles", y = "Average Ranking") +
  theme_minimal()
plot_2

ggExtra::ggMarginal(p = plot_2, type = "histogram")
ggExtra::ggMarginal(p = plot_2, type = "density")
ggExtra::ggMarginal(p = plot_2, type = "boxplot")
ggExtra::ggMarginal(p = plot_2, type = "violin")
ggExtra::ggMarginal(p = plot_2, type = "densigram")


