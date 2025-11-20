

library(tidyverse)
library(here)

# Read in the processed ATP singles data
atp_singles_data <- readRDS(
  file = here::here("data", "processed", "atp_singles.Rds")
)

# Choose the countries you're focusing on
selected_countries <- c("USA", "ESP", "FRA", "ITA", "GER")

# Summarize wins per year per country
wins_selected <- atp_singles_data %>%
  mutate(
    year = as.integer(format(as.Date(tourney_date), "%Y")),
    winner_ioc = toupper(str_trim(winner_ioc))
  ) %>%
  filter(
    !is.na(year),
    year >= 1968,
    winner_ioc %in% selected_countries
  ) %>%
  count(year, winner_ioc, name = "wins") %>%
  rename(country = winner_ioc)

# Save the cleaned / summarized data for plotting later
saveRDS(
  wins_selected,
  here::here("data", "processed", "wins_selected.rds")
)
