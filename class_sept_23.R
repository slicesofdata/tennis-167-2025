################################################################
# Script Name: carly_baretz_data_cleaning.R
# Author: Carly Baretz
# GitHub: carlybaretz
# Date Created: september 23, 2025
#
# Purpose: Visualizing Associations and Mapping Aesthetics
#
################################################################
# Load necessary libraries/source any function directories
library(tidyverse)
library(ggplot2)
library(readr)
library(here)

################################################################
# data reading
cleaned_ranking <- readRDS(file = here::here("data", "processed", "cleaned_rankings_data.rds"))

glimpse(cleaned_ranking)
################################################################
#Plotting

cleaned_ranking |>
  filter(ioc == "USA" ) |>
  ggplot( mapping = aes(x = ioc, y = rank))+
            geom_point()

cleaned_ranking_numeric_var <- cleaned_ranking |>
  filter(ioc == "USA", !is.na(height)) |>
  ggplot(aes(x = ioc, y = rank, color = height)) +
  geom_point()
################################################################
#plotting - mapping a non-numeric variable to color
mapping_non_num_color <- cleaned_ranking |>

  filter(ioc %in% ("USA", "GER", "FRA", "AUS"))
  ggplot(aes(x = ioc, y = rank, color = ioc)) +
  geom_point()


part2_plot <- cleaned_ranking |>
  filter(!is.na(rank), rank < 100,
         ioc == "USA" | ioc == "ECU" | ioc == "AUS") |>
  ggplot(aes(x = ranking_date, y = rank, color = ioc)) +
  geom_point()

part2_plot
# plot 2 notes - this graph looks insane, don't graph ranking_date on the x
################################################################

#plotting - using the hand variable as color

part3_plot_hand <- cleaned_ranking |>
  filter(!is.na(rank), rank < 100,
         ioc == "USA" | ioc == "AUS") |>
  ggplot(aes(x = ranking_date, y = rank, color = hand)) +
  geom_point()
part3_plot_hand

# i like the right vs left distinction, this may be of interest to graph
