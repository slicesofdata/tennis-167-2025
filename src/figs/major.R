library(dplyr)
library(tidyverse)
library(here)
library(tidyr)
library(ggplot2)
library(scales)

save_plot_png <- function(
    plot,
    file_name,
    figs_dir,
    units = "px",
    width = 1600,
    height = 1100,
    dpi = 300) {

  file_path <- here::here(figs_dir, file_name)
  print(file_path)
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


player_colors <- c(
  "Federer" = "#006400",   # Green
  "Nadal" = "#D32F2F",     # Red
  "Djokovic" = "#1976D2",  # Blue
  "Alcaraz" = "#FF8C00",   # Orange
  "Sinner" = "#9C27B0"     # Pink
)

################################################################################

cleaned_rankings_data <- readRDS(
  here::here("data", "processed", "cleaned_rankings_data.rds")
)

atp_singles_data <- readRDS(
  here::here("data", "processed", "atp_singles.Rds")
)

#################################################################################

players <- c(
  "Roger Federer",
  "Rafael Nadal",
  "Novak Djokovic",
  "Jannik Sinner",
  "Carlos Alcaraz"
)

filtered_singles <- atp_singles_data %>%
  filter(
    winner_name %in% players | loser_name %in% players
  ) %>%
  select(
    tourney_level,
    winner_name,
    loser_name
  ) %>%
  mutate(
    tourney_type = if_else(tourney_level == "G", "Major", "Non-Major")
  )

##########################################################################

player_match_outcomes <- filtered_singles %>%
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) %>%
  mutate(
    is_win = if_else(role == "winner_name", 1L, 0L)
  )

win_pct_by_type <- player_match_outcomes %>%
  filter(player %in% players) %>%
  group_by(player, tourney_type) %>%
  summarise(
    matches_played = n(),
    matches_won    = sum(is_win),
    win_pct        = matches_won / matches_played,
    .groups = "drop"
  ) %>%
  mutate(
    player_short = case_when(
      player == "Roger Federer"   ~ "Federer",
      player == "Rafael Nadal"    ~ "Nadal",
      player == "Novak Djokovic"  ~ "Djokovic",
      player == "Jannik Sinner"   ~ "Sinner",
      player == "Carlos Alcaraz"  ~ "Alcaraz",
      TRUE ~ player
    ),
    player_short = factor(
      player_short,
      levels = c("Federer", "Nadal", "Djokovic", "Sinner", "Alcaraz")
    ),
    # make sure tourney_type is ordered nicely
    tourney_type = factor(tourney_type, levels = c("Major", "Non-Major"))
  )

##########################################################################

majorsviz <- ggplot(
  win_pct_by_type,
  aes(
    x     = player_short,
    y     = win_pct,
    fill  = player_short,
    alpha = tourney_type,
    group = tourney_type
  )
) +
  geom_col(position = position_dodge(width = 0.75)) +

  # player colors from your vector
  scale_fill_manual(
    values = player_colors,
    name   = "Player"
  ) +

  # alpha levels & legend for Major vs Non-Major
  scale_alpha_manual(
    values = c("Major" = 1, "Non-Major" = 0.4)
  ) +

  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.10),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +

  labs(
    x     = NULL,
    y     = "Singles Matches Won (%)",
    title = "Winning Percentage in Majors vs. Non-Majors",
    caption = "Solid colors = Majors; Faded colors = Non-Majors"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(
    hjust = 0.5,
    size = 10,
    face = "italic",
    margin = margin(t = 10)
  ))

majorsviz

###############################################################################

save_plot_png(
  plot = majorsviz,
  file_name = "majorswinp.png",
  figs_dir = "figs"
)

########################################################################

filtered_singles_finals <- atp_singles_data %>%
  filter(
    winner_name %in% players,  # only care about these players as winners
    round == "F"               # final championship round only
  ) %>%
  select(
    tourney_level,
    winner_name
  ) %>%
  mutate(
    tourney_type = if_else(tourney_level == "G", "Major", "Non-Major")
  )



titles_total <- filtered_singles_finals %>%
  group_by(winner_name) %>%
  summarise(
    titles_total = n(),
    .groups = "drop"
  ) %>%
  mutate(
    player_short = case_when(
      winner_name == "Roger Federer"   ~ "Federer",
      winner_name == "Rafael Nadal"    ~ "Nadal",
      winner_name == "Novak Djokovic"  ~ "Djokovic",
      winner_name == "Jannik Sinner"   ~ "Sinner",
      winner_name == "Carlos Alcaraz"  ~ "Alcaraz",
      TRUE ~ winner_name
    ),
    player_short = factor(
      player_short,
      levels = c("Federer", "Nadal", "Djokovic", "Sinner", "Alcaraz")
    )
  )


titles_by_type <- filtered_singles_finals %>%
  group_by(winner_name, tourney_type) %>%
  summarise(
    titles = n(),
    .groups = "drop"
  ) %>%
  mutate(
    player_short = case_when(
      winner_name == "Roger Federer"   ~ "Federer",
      winner_name == "Rafael Nadal"    ~ "Nadal",
      winner_name == "Novak Djokovic"  ~ "Djokovic",
      winner_name == "Jannik Sinner"   ~ "Sinner",
      winner_name == "Carlos Alcaraz"  ~ "Alcaraz",
      TRUE ~ winner_name
    ),
    player_short = factor(
      player_short,
      levels = c("Federer", "Nadal", "Djokovic", "Sinner", "Alcaraz")
    ),
    tourney_type = factor(tourney_type, levels = c("Major", "Non-Major"))
  )




titles_viz <- ggplot(
  titles_by_type,
  aes(
    x     = player_short,
    y     = titles,
    fill  = player_short,   # player colors
    alpha = tourney_type,   # Major vs Non-Major
    group = tourney_type
  )
) +
  geom_col(position = position_dodge(width = 0.75)) +
  geom_text(
    aes(
      x = player_short,
      y = titles,
      label = titles,
      group = tourney_type
    ),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 4,
    color = "black",
    inherit.aes = FALSE
  ) +

  # Player colors, no player legend
  scale_fill_manual(
    values = player_colors,
    guide  = "none"
  ) +

  # Keep only the tourney_type legend
  scale_alpha_manual(
    values = c("Major" = 1, "Non-Major" = 0.4)
  ) +

  labs(
    x     = NULL,
    y     = "Number of Titles Won",
    title = "Total Singles Tournaments Won",
    subtitle = "Majors vs. Non-Majors",
    caption = "Solid colors = Major Wins; Faded colors = Non-Major Wins"
  ) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.position = "top",
    legend.title    = element_text(face = "bold"),
    legend.key.width = unit(1.5, "cm")
  ) + theme(legend.position = "none") +
  theme(plot.caption = element_text(
    hjust = 0.5,
    size = 10,
    face = "italic",
    margin = margin(t = 10)
  ))
titles_viz

save_plot_png(
  plot = titles_viz,
  file_name = "majortitles.png",
  figs_dir = "figs"
)




