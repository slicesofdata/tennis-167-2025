################################################################################
# Script Name: Exercise: Visualizing Associations and Mapping Aesthetics
# Author: Lindsay Eisenman
# GitHub: lindsayeisenman
# Date Created: 9/23/25
#
# Purpose: This script looks at our cleaned doubles data.
#
################################################################################
# Load necessary libraries and read file
library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)

cleaned_doubles_data <- readRDS(file = here::here("data","processed","cleaned_doubles_data.Rds"))
glimpse(cleaned_doubles_data)

################################################################################
# Data Cleaning for Plot 1

point_plot_cleaned_data <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  mutate(winner1_ht = as.numeric(winner1_ht)) %>%
  mutate(winner2_ht = as.numeric(winner2_ht)) %>%
  mutate(loser1_ht = as.numeric(loser1_ht)) %>%
  mutate(loser2_ht = as.numeric(loser2_ht)) %>%
  mutate(winner1_age = as.numeric(winner1_age)) %>%
  mutate(winner2_age = as.numeric(winner2_age)) %>%
  mutate(loser1_age = as.numeric(loser1_age)) %>%
  mutate(loser2_age = as.numeric(loser2_age)) %>%
  pivot_longer(cols = c(winner1_age, winner2_age, loser1_age, loser2_age), 
               names_to = "player_age", values_to = "age") %>%
  pivot_longer(cols = c(winner1_rank, winner2_rank, loser1_rank, loser2_rank), 
               names_to = "player_rank", values_to = "rank") %>%
  pivot_longer(cols = c(winner1_ht, winner2_ht, loser1_ht, loser2_ht), 
               names_to = "player_ht", values_to = "ht") %>%
  filter(rank < 11) %>%
  filter(year(tourney_date) == 2019)
point_plot_cleaned_data

################################################################################
# Numeric Point Plot

#This data looks at the associated between the rank and age of top doubles players in the year 2020.
#It also adds a third dimension, height, to see if there is a correlation between higher ranked doubles players and height.

point_plot <- point_plot_cleaned_data %>%
  ggplot(mapping = aes(x = age, y = rank, color = ht)) +
  geom_point()
point_plot

################################################################################
# Data Cleaning for Plot 2

point_plot2_cleaned_data <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  mutate(winner1_ht = as.numeric(winner1_ht)) %>%
  mutate(winner2_ht = as.numeric(winner2_ht)) %>%
  mutate(loser1_ht = as.numeric(loser1_ht)) %>%
  mutate(loser2_ht = as.numeric(loser2_ht)) %>%
  mutate(winner1_age = as.numeric(winner1_age)) %>%
  mutate(winner2_age = as.numeric(winner2_age)) %>%
  mutate(loser1_age = as.numeric(loser1_age)) %>%
  mutate(loser2_age = as.numeric(loser2_age)) %>%
  pivot_longer(cols = c(winner1_age, winner2_age, loser1_age, loser2_age), 
               names_to = "player_age", values_to = "age") %>%
  pivot_longer(cols = c(winner1_rank, winner2_rank, loser1_rank, loser2_rank), 
               names_to = "player_rank", values_to = "rank") %>%
  pivot_longer(cols = c(winner1_ht, winner2_ht, loser1_ht, loser2_ht), 
               names_to = "player_ht", values_to = "ht") %>%
  filter(year(tourney_date) == 2019)
point_plot2_cleaned_data
################################################################################
# Non-numeric Point Plot

#This graph relates the surface and tournament date variables. 
#It shows what time of year different surfaces are played on.

point_plot_non_numeric <- point_plot2_cleaned_data %>%
  ggplot(mapping = aes(x = tourney_date, y = surface)) +
  geom_point()
point_plot_non_numeric

################################################################################
# Point Plot with Color

#The color variable shows the tournament level. "G" is the grandslam, and we can see 4 of these colored dots.

point_plot_non_numeric <- point_plot2_cleaned_data %>%
  ggplot(mapping = aes(x = tourney_date, y = surface, color = tourney_level)) +
  geom_point()
point_plot_non_numeric

################################################################################
# End of script
