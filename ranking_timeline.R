library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)

#######################################################################################################################

cleaned_rankings_data <- readRDS(file = here::here("data","processed","cleaned_rankings_data.rds")) 
head(cleaned_rankings_data)

#######################################################################################################################

cleaneddata_federer <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Federer") %>%
  filter(name_first == "Roger")
cleaneddata_federer

federer_ranking_timeline <- cleaneddata_federer %>%
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_line() +
  labs(x = "Date", y = "Ranking") +
  theme_minimal()
federer_ranking_timeline

#######################################################################################################################

cleaneddata_nadal <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Nadal") %>%
  filter(name_first == "Rafael")
cleaneddata_nadal

nadal_ranking_timeline <- cleaneddata_nadal %>%
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_line() +
  labs(x = "Date", y = "Ranking") +
  theme_minimal()
nadal_ranking_timeline

#######################################################################################################################

cleaneddata_djokovic <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Djokovic") %>%
  filter(name_first == "Novak")
cleaneddata_djokovic

djokovic_ranking_timeline <- cleaneddata_djokovic %>%
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_line() +
  labs(x = "Date", y = "Ranking") +
  theme_minimal()
djokovic_ranking_timeline

#######################################################################################################################

cleaneddata_sinner <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Sinner") %>%
  filter(name_first == "Jannik")
cleaneddata_sinner

sinner_ranking_timeline <- cleaneddata_sinner %>%
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_line() +
  labs(x = "Date", y = "Ranking") +
  theme_minimal()
sinner_ranking_timeline

#######################################################################################################################

cleaneddata_alcaraz <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Alcaraz") %>%
  filter(name_first == "Carlos")
cleaneddata_alcaraz

alcaraz_ranking_timeline <- cleaneddata_alcaraz %>%
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_line() +
  labs(x = "Date", y = "Ranking") +
  theme_minimal()
alcaraz_ranking_timeline
