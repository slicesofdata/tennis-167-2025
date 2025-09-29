library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)

#######################################################################################################################

cleaned_doubles_data <- readRDS(file = here::here("data","processed","cleaned_doubles_data.Rds")) 
head(cleaned_doubles_data)

#######################################################################################################################

doubles_federer <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter(winner1_name == "Roger Federer" | winner2_name == "Roger Federer" 
         | loser1_name == "RogerFederer" | loser2_name == "Roger Federer") %>%
  pivot_longer(cols = c(winner1_name, winner2_name, loser1_name, loser2_name), 
               names_to = "player", values_to = "name") %>%
  filter(name == "Roger Federer") %>%
  mutate(rank = case_when(
    player == "winner1_name" ~ winner1_rank,
    player == "winner2_name" ~ winner2_rank,
    player == "loser1_name" ~ loser1_rank,
    player == "loser2_name" ~ loser2_rank))
doubles_federer

doubles_federer_graph <- doubles_federer %>%
  ggplot(mapping = aes(x = tourney_date, y = rank)) +
  geom_line() +
  labs(x = "Date", y = "Ranking")
doubles_federer_graph

#######################################################################################################################

doubles_nadal <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter(winner1_name == "Rafael Nadal" | winner2_name == "Rafael Nadal" 
         | loser1_name == "Rafael Nadal" | loser2_name == "Rafael Nadal") %>%
  pivot_longer(cols = c(winner1_name, winner2_name, loser1_name, loser2_name), 
               names_to = "player", values_to = "name") %>%
  filter(name == "Rafael Nadal") %>%
  mutate(rank = case_when(
    player == "winner1_name" ~ winner1_rank,
    player == "winner2_name" ~ winner2_rank,
    player == "loser1_name" ~ loser1_rank,
    player == "loser2_name" ~ loser2_rank))
doubles_nadal

doubles_nadal_graph <- doubles_nadal %>%
  ggplot(mapping = aes(x = tourney_date, y = rank)) +
  geom_line() +
  labs(x = "Date", y = "Ranking")
doubles_nadal_graph

#######################################################################################################################

doubles_djokovic <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter(winner1_name == "Novak Djokovic" | winner2_name == "Novak Djokovic" 
         | loser1_name == "Novak Djokovic" | loser2_name == "Novak Djokovic") %>%
  pivot_longer(cols = c(winner1_name, winner2_name, loser1_name, loser2_name), 
               names_to = "player", values_to = "name") %>%
  filter(name == "Novak Djokovic") %>%
  mutate(rank = case_when(
    player == "winner1_name" ~ winner1_rank,
    player == "winner2_name" ~ winner2_rank,
    player == "loser1_name" ~ loser1_rank,
    player == "loser2_name" ~ loser2_rank))
doubles_djokovic

doubles_djokovic_graph <- doubles_djokovic %>%
  ggplot(mapping = aes(x = tourney_date, y = rank)) +
  geom_line() +
  labs(x = "Date", y = "Ranking")
doubles_djokovic_graph