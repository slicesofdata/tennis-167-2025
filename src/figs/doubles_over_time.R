library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)


player_colors <- c(
  "Federer" = "#006400",   # Green
  "Nadal" = "#D32F2F",     # Red
  "Djokovic" = "#1976D2",  # Blue
  "Alcaraz" = "#FF8C00",   # Orange
  "Sinner" = "#9C27B0"     # Pink
)

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

cleaned_doubles_data <- readRDS(file = here::here("data","processed","cleaned_doubles_data.Rds")) 
head(cleaned_doubles_data)

atp_singles_data <- readRDS(file = here::here("data","processed","atp_singles.Rds")) 
head(atp_singles_data)

#######################################################################################################################

#grand slam wins

first_grand_slam <- atp_singles_data %>%
  filter(
    winner_name %in% c("Roger Federer", "Rafael Nadal", "Novak Djokovic") 
  ) %>%
  filter (tourney_level == "G") %>%
  filter(round == "F") %>%
  select(tourney_name, tourney_date, winner_name, winner_rank) %>%
  arrange(winner_name, tourney_date) %>%
  group_by(winner_name) %>%
  slice_min(tourney_date) %>%
  ungroup() %>%
  mutate(year_of_first_win = as.numeric(format(tourney_date, "%Y"))) %>%
  select(winner_name, year_of_first_win)
first_grand_slam

#######################################################################################################################

#this graph is going to show the dates that these players were playing doubles tournaments
#hopefully to show that it was only early in their career, and that they focused on playing singles


doubles_matches_long <- cleaned_doubles_data %>%
  mutate(
    tourney_date = as.Date(tourney_date, format = "%m/%d/%Y"),
    year = year(tourney_date)
  ) %>%
  select(year, winner1_name, winner2_name, loser1_name, loser2_name) %>%
  pivot_longer(
    cols = c(winner1_name, winner2_name, loser1_name, loser2_name),
    names_to = "role",
    values_to = "player"
  ) %>%
  filter(player %in% c("Roger Federer", "Novak Djokovic", "Rafael Nadal", "Jannik Sinner", "Carlos Alcaraz"))
doubles_matches_long


matches_per_player <- doubles_matches_long %>%
  group_by(year, player) %>%
  summarize(matches_played = n(), .groups = "drop")
matches_per_player

matches_per_player$player <- factor(matches_per_player$player, 
                                    levels = c("Roger Federer", 
                                               "Rafael Nadal", 
                                               "Novak Djokovic", 
                                               "Carlos Alcaraz", 
                                               "Jannik Sinner"))

#######################################################################################################################

doubles_matches_plot <- matches_per_player %>%
  ggplot(aes(x = year, y = matches_played, color = player)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  labs(
    title = "Number of Doubles Matches Played per Year",
    x = "Year",
    y = "Matches Played",
    color = " ",
    caption = "Dotted lines represent the year each player won their first singles grand slam.
After achieving this milestone, the need to play doubles generally declined."
  ) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_color_manual(values = c(
    "Roger Federer" = "#006400",  
    "Rafael Nadal" = "#D32F2F",    
    "Novak Djokovic" = "#1976D2",  
    "Carlos Alcaraz" = "#FF8C00",  
    "Jannik Sinner" = "#9C27B0"    
  )) +
  geom_vline(data = first_grand_slam, 
             aes(xintercept = year_of_first_win, color = winner_name),
             linetype = "dotted", size = 1, alpha = 0.7, show.legend = FALSE) +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5, face = "italic"))
doubles_matches_plot


save_plot_png(plot = doubles_matches_plot, file_name = "doubles_played_per_year.png", figs_dir = "figs")




