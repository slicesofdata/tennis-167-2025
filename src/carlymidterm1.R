

#midterm graph 1

#US PLAYERS AND HOW THEY DO AT THE US OPEN VS AUSTRALIAN OPEN
#research q - were also looking at other varibales

########################################################################
# load data/ libs

library(tidyverse)
library(ggplot2)
library(readr)
library(here)


atp_singles_data <- readRDS(file = here :: here("data","processed","atp_singles.Rds" ))
head(atp_singles_data)

########################################################################
# graph 1



########################################################################
# graph 2


# Define countries of interest
selected_countries <- c("USA", "ESP", "FRA", "ITA", "GER")

# Summarize wins per year per country
graph2 <- wins_selected <- atp_singles_data %>%
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

# Multi-line plot for only the selected countries
ggplot(wins_selected, aes(x = year, y = wins, color = country)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  labs(
    title = "Match Wins by Country Over Time ",
    subtitle = "USA, Spain, France, Italy, and Germany (1968–2025)",
    x = "Year",
    y = "Number of Match Wins",
    color = "Country"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

graph2
########################################
















atp_singles_data %>%
  # clean tournament names
  mutate(
    year = as.integer(format(as.Date(tourney_date), "%Y")),
    tn_clean = tourney_name |>
      str_to_lower() |>
      str_replace_all("\\.", "") |>
      str_squish(),
    tourney = case_when(
      str_detect(tn_clean, "^us\\s*open$") ~ "US Open",
      str_detect(tn_clean, "^australian\\s*open$") ~ "Australian Open",
      TRUE ~ NA_character_
    ),
    winner_ioc = toupper(winner_ioc),
    loser_ioc  = toupper(loser_ioc)
  ) %>%
  # filter relevant years, tournaments, and American matches
  filter(
    year >= 2000, year <= 2025,
    !is.na(tourney),
    winner_ioc == "USA" | loser_ioc == "USA"
  ) %>%
  mutate(american_win = (winner_ioc == "USA")) %>%
  group_by(tourney, year) %>%
  summarise(
    win_percent = 100 * mean(american_win, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # line graph instead of bar chart
  ggplot(aes(x = year, y = win_percent, color = tourney)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
 # geom_smooth(se = FALSE, method = "loess", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "American Player Win %: US Open vs Australian Open (2000–2025)",
    subtitle = "Visualizing home-court advantage over time",
    x = "Year",
    y = "Win Percentage (%)",
    color = "Tournament"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
