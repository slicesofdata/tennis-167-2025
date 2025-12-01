library(dplyr)
library(tidyverse)
library(here)
library(tidyr)
library(ggplot2)
library(scales)

getwd()

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


player_colors <- c(
  "Federer" = "#006400",   # Green
  "Nadal" = "#D32F2F",     # Red
  "Djokovic" = "#1976D2",  # Blue
  "Alcaraz" = "#FF8C00",   # Orange
  "Sinner" = "#9C27B0"     # Pink
)

################################################################################

cleaned_rankings_data <- readRDS("data/processed/cleaned_rankings_data.rds")

atp_singles_data <- readRDS("data/processed/atp_singles.Rds") 

#################################################################################

players <- c("Roger Federer", "Novak Djokovic", "Rafael Nadal", 
             "Jannik Sinner", "Carlos Alcaraz")

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

################################################################################

player_match_outcomes <- filtered_singles %>%
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) %>%
  mutate(
    is_win = if_else(role == "winner_name", 1L, 0L)
  )

##############################################################################


win_pct_by_type <- player_match_outcomes %>%
  filter(player %in% players) %>%
  group_by(player, tourney_type) %>%
  summarise(
    matches_played = n(),
    matches_won    = sum(is_win),
    win_pct        = matches_won / matches_played,
    .groups = "drop"
  )



################################################################################


majorsviz <- ggplot(win_pct_by_type,
       aes(x = player, y = win_pct, fill = tourney_type)) +
  
  geom_col(position = position_dodge(width = 0.75)) +
  
  scale_fill_manual(
    values = c("Major" = "#1b9e77", "Non-Major" = "#7570b3"),
    name   = "Tournament Type",
    labels = c("Major (Grand Slams)", "All Other Events")
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = NULL,
    y = "Winning Percentage",
    title = "Winning Percentage in Majors vs Non-Majors for Five Focus Players",
    subtitle = "Comparison across Grand Slams and all other tournament levels",
    caption = "Source: ATP Match Records"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      face = "bold"
    ),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(1.5, "cm")
  )


###############################################################################

save_plot_png(
  plot = majorsviz,
  file_name = "majorsurface.png",
  figs_dir = "figs"
)


##############################################################################


filtered_surface <- atp_singles_data %>%
  filter(
    winner_name %in% players | loser_name %in% players
  ) %>%
  select(
    surface,
    winner_name,
    loser_name
  ) %>%
  filter(!is.na(surface), surface != "Carpet")


#####################################################################################


player_surface_outcomes <- filtered_surface %>%
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) %>%
  mutate(
    is_win = if_else(role == "winner_name", 1L, 0L)
  ) %>%
  filter(player %in% players)


###################################################################################

win_pct_by_surface <- player_surface_outcomes %>%
  group_by(player, surface) %>%
  summarise(
    matches_played = n(),
    matches_won    = sum(is_win),
    win_pct        = matches_won / matches_played,
    .groups = "drop"
  )

win_pct_by_surface <- win_pct_by_surface %>%
  mutate(
    player_short = case_when(
      player == "Roger Federer"   ~ "Federer",
      player == "Rafael Nadal"    ~ "Nadal",
      player == "Novak Djokovic"  ~ "Djokovic",
      player == "Carlos Alcaraz"  ~ "Alcaraz",
      player == "Jannik Sinner"   ~ "Sinner",
      TRUE ~ player
    )
  )


##################################################################################


surfaceviz <- ggplot(win_pct_by_surface,
       aes(x = player_short, y = win_pct, fill = surface)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)  # optional: lock 0–100%
  ) +
  scale_fill_manual(
    values = c(
      "Hard"  = "#1f77b4",  # blue
      "Clay"  = "#d62728",  # red
      "Grass" = "#2ca02c"   # green
    ),
    guide = guide_legend(
      title = "Court Surface",
      override.aes = list(alpha = 0.9),
      nrow = 1,            # legend in a single row
      byrow = TRUE
    )
  ) +
  labs(
    x = NULL,
    y = "Winning Percentage",
    title = "Winning Percentage by Surface",
    subtitle = "Roger Federer · Rafael Nadal · Novak Djokovic · Jannik Sinner · Carlos Alcaraz",
    caption = "Source: ATP singles match data"
  ) +
  theme_minimal(base_size = 13) +
  theme(
  
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10, colour = "grey30"),
    plot.caption = element_text(size = 8, colour = "grey40"),
    
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.box = "horizontal"
  )


###################################################################

save_plot_png(
  plot = surfaceviz,
  file_name = "surfacewins.png",
  figs_dir = "figs"
)


