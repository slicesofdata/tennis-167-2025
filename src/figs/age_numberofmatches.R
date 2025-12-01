library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)


save_plot_png <- function(
    plot,
    file_name, 
    figs_dir,
    units = "px",
    width = 1600,
    height = 1100,
    dpi = 300) {
  
  file_path <- here::here(figs_dir, file_name)
  
  ggsave(
    filename = file_path,
    plot = plot,
    device = "png",
    units = units,
    width = width,
    height = height,
    dpi = dpi
  )
}

#######################################################################################################################

atp_singles_data <- readRDS(file = here::here("data","processed","atp_singles.Rds")) 
head(atp_singles_data)

#######################################################################################################################

players <- c("Roger Federer", "Novak Djokovic", "Rafael Nadal",
             "Jannik Sinner", "Carlos Alcaraz") 


player_colors <- c(
  "Roger Federer"  = "#006400",  # Green
  "Rafael Nadal"   = "#D32F2F",  # Red
  "Novak Djokovic" = "#1976D2",  # Blue
  "Carlos Alcaraz" = "#FF8C00",  # Orange
  "Jannik Sinner"  = "#9C27B0"   # Pink/Purple
)


matches_per_year <- atp_singles_data %>%
  filter(winner_name %in% players | loser_name %in% players) %>%
  mutate(year = year(tourney_date)) %>%
  select(player_name = winner_name, age = winner_age, year) %>%
  bind_rows(
    atp_singles_data %>%
      filter(winner_name %in% players | loser_name %in% players) %>%
      mutate(year = year(tourney_date)) %>%
      select(player_name = loser_name, age = loser_age, year)
  ) %>%
  filter(player_name %in% players) %>%
  group_by(player_name, year) %>%
  summarise(
    matches = n(),
    age = median(age),  
    .groups = "drop"
  ) %>%
  arrange(player_name, year)
matches_per_year


matches_per_year$player_name <- factor(
  matches_per_year$player_name,
  levels = c(
    "Roger Federer",
    "Rafael Nadal",
    "Novak Djokovic",
    "Jannik Sinner",
    "Carlos Alcaraz"
  )
)



ggplot(matches_per_year, aes(x = age, y = matches, fill = player_name)) +
  geom_col( ) +  
  scale_fill_manual(values = player_colors) +
  facet_wrap(~ player_name) + 
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    title = "Number of Matches Played by Age per Player",
    x = "Age",
    y = "Number of Matches",
    fill = "Player"
  )


ggplot(matches_per_year, aes(x = age, y = player_name, height = matches, fill = player_name)) +
  geom_density_ridges2(stat = "identity", scale = 1, alpha = 0.75) +
  scale_fill_manual(values = player_colors) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    title = "Ridge Plot: Number of Matches Played by Age per Player",
    x = "Age",
    y = " ",
    fill = "Player"
  )








