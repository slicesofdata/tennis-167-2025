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

#number of matches played in signles and doubles

doubles_matches_number <- cleaned_doubles_data %>%
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
  filter(player %in% c("Roger Federer", "Novak Djokovic", "Rafael Nadal", "Jannik Sinner", "Carlos Alcaraz")) %>%
  group_by(player) %>%
  summarise(matches_played = n()) %>%
  arrange(desc(matches_played))
doubles_matches_number



singles_matches_number <- atp_singles_data %>%
  mutate(
    tourney_date = as.Date(tourney_date, format = "%m/%d/%Y"),
    year = year(tourney_date)
  ) %>%
  select(year, winner_name, loser_name) %>%
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) %>%
  filter(player %in% c("Roger Federer", "Novak Djokovic", "Rafael Nadal", "Jannik Sinner", "Carlos Alcaraz")) %>%
  group_by(player) %>%
  summarise(matches_played = n()) %>%
  arrange(desc(matches_played))
singles_matches_number


combined_matches_number <- doubles_matches_number %>%
  full_join(singles_matches_number, by = "player", suffix = c("_doubles", "_singles")) %>%
  replace_na(list(matches_played_doubles = 0, matches_played_singles = 0)) %>%
  arrange(desc(matches_played_doubles + matches_played_singles))
combined_matches_number

matches_long <- combined_matches_number %>%
  pivot_longer(cols = c(matches_played_singles, matches_played_doubles),
               names_to = "match_type", 
               values_to = "matches_played") %>%
  mutate(match_type = factor(match_type, levels = c("matches_played_singles", "matches_played_doubles")))
matches_long

#######################################################################################################################


singles_vs_doubles_plot <- ggplot(matches_long, aes(x = forcats::fct_reorder(.f = player, 
                                                                                 .x = matches_played, 
                                                                                 .fun = min,
                                                                                 .desc = T),, y = matches_played, 
                                                    fill = interaction(player, match_type))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, max(matches_long$matches_played), by = 250)) +
  scale_fill_manual(
    values = c(
      "Roger Federer.matches_played_doubles" = alpha("#006400", 0.5),   
      "Roger Federer.matches_played_singles" = "#006400",
      "Rafael Nadal.matches_played_doubles"   = alpha("#D32F2F", 0.5),
      "Rafael Nadal.matches_played_singles"  = "#D32F2F",
      "Novak Djokovic.matches_played_doubles" = alpha("#1976D2", 0.5),
      "Novak Djokovic.matches_played_singles"= "#1976D2",
      "Carlos Alcaraz.matches_played_doubles" = alpha("#FF8C00", 0.5),
      "Carlos Alcaraz.matches_played_singles"= "#FF8C00",
      "Jannik Sinner.matches_played_doubles"  = alpha("#9C27B0", 0.5),
      "Jannik Sinner.matches_played_singles" = "#9C27B0"
    )
  ) +
  labs(title = "Singles vs Doubles Matches Played",
       x = " ",
       y = "Number of Matches Played",
       caption = "Solid colors = Singles Matches; Faded colors = Doubles Matches") +
  geom_text(aes(label = scales::comma(matches_played)), 
            position = position_dodge(width = 0.7),  
            vjust = -0.5, size = 3, color = "#4A4A4A") + 
  theme_classic() +
  theme(legend.position = "none",
        plot.caption = element_text(
          hjust = 0.5,      # centers the caption horizontally
          size = 10,        # adjusts font size
          face = "italic",  # optional, makes it italic
          margin = margin(t = 10)))
singles_vs_doubles_plot


save_plot_png(plot = singles_vs_doubles_plot, file_name = "singles_vs_doubles.png", figs_dir = "figs")


#######################################################################################################################




