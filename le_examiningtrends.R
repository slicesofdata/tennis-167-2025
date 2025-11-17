library(dplyr)
library(tidyverse)
library(ggplot2)
library(here)

#######################################################################################################################

cleaned_rankings_data <- readRDS(file = here::here("data","processed","cleaned_rankings_data.rds")) 
head(cleaned_rankings_data)

atp_singles_data <- readRDS(file = here::here("data","processed","atp_singles.Rds")) 
head(atp_singles_data)

#######################################################################################################################

filtered_atp_singles <- atp_singles_data %>%
  filter (tourney_level == "G") %>%
  filter(round == "F") %>%
  select(tourney_date, winner_age, surface)
filtered_atp_singles

#######################################################################################################################

plot1 <- filtered_atp_singles %>%
  ggplot(mapping = aes(x = tourney_date, y = winner_age)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  #geom_smooth(method = "loess", se = FALSE, color = "red") +
  geom_smooth(method = "loess", se = FALSE, aes(color = "Span = 0.2"), span = 0.2) +
  geom_smooth(method = "loess", se = FALSE, aes(color = "Span = 0.5"), span = 0.5) +
  geom_smooth(method = "loess", se = FALSE, aes(color = "Span = 0.9"), span = 0.9) +
  scale_color_manual(
    name = "LOESS Span",
    values = c("Span = 0.2" = "red", "Span = 0.5" = "orange", "Span = 0.9" = "green")) +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  labs(x = "Tournament Date", y = "Age (years)", title = "Age of Grand Slam Winners over Time") +
  theme_minimal()
plot1
  
#######################################################################################################################
  
plot2 <- filtered_atp_singles %>%
  ggplot(mapping = aes(x = tourney_date, y = winner_age, color = surface)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  labs(x = "Tournament Date", y = "Age (years)", title = "Age of Grand Slam Winners over Time",
       color = "Surface Type") +
  theme_minimal()
plot2
  
#######################################################################################################################
