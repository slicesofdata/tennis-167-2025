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

cleaned_rankings_data <- readRDS("data/processed/cleaned_rankings_data.rds")

atp_singles_data <- readRDS("data/processed/atp_singles.Rds") 

#################################################################################

players <- c(
  "Roger Federer",
  "Rafael Nadal",
  "Novak Djokovic",
  "Jannik Sinner",
  "Carlos Alcaraz"
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

desired_order <- c("Federer", "Nadal", "Djokovic", "Sinner", "Alcaraz")

win_pct_by_surface <- win_pct_by_surface %>%
  mutate(player_short = factor(player_short, levels = desired_order))


surfaceviz <- ggplot(win_pct_by_surface,
       aes(x = player_short, y = win_pct, fill = surface)) +
  geom_col(
    position = position_dodge(width = 0.6),
    width = 0.5, alpha = 0.9
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.10)
  ) +
  scale_fill_manual(
    values = c(
      "Hard"  = "#0077C8",  # Australian Open blue
      "Clay"  = "#E45C2B",  # Roland-Garros clay orange
      "Grass" = "#43A047"   # Wimbledon green
    ),
    guide = guide_legend(
      title = "Court Surface"
    )
  ) +
  labs(
    x = NULL,
    y = "Matches Won (%)",
    title = "Percentage of Singles Matches Won on Each Surface"
  ) +
  theme_minimal() +
  theme(
  
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10, colour = "grey30"),
    plot.caption = element_text(size = 8, colour = "grey40"),
    
    
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.box = "vertical"
  )

###################################################################

save_plot_png(
  plot = surfaceviz,
  file_name = "surfacewinp.png",
  figs_dir = "figs"
)

##############################################################################

titles_by_surface <- atp_singles_data %>%
  filter(
    winner_name %in% players,   # same players
    round == "F",               # finals only
    surface != "Carpet"         # remove carpet
  ) %>%
  group_by(winner_name, surface) %>%
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
    # reorder surfaces without Carpet
    surface = factor(surface, levels = c("Hard", "Clay", "Grass"))
  )


titles_by_surface_pct <- titles_by_surface %>%
  group_by(player_short) %>%
  mutate(
    total_titles = sum(titles),
    pct_titles   = titles / total_titles
  ) %>%
  ungroup()

titles_surface_share_viz <- ggplot(
  titles_by_surface_pct,
  aes(
    x    = player_short,
    y    = pct_titles,
    fill = surface
  )
) +
  geom_col(width = 0.5, alpha = 0.65) +   # slightly transparent bars
  
  geom_text(
    aes(label = titles),
    position = position_stack(vjust = 0.5),  # center of each stacked chunk
    color = "black",
    size = 3.5
  )  +   # stacked by default
  
  scale_fill_manual(
    values = c(
      "Hard"  = "#0077C8",  # Australian Open blue
      "Clay"  = "#E45C2B",  # Roland-Garros clay orange
      "Grass" = "#43A047"   # Wimbledon green
    ),
    name = "Surface"
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    x     = NULL,
    y     = "Tournaments Won by Surface (%)",
    title = "Singles Tournaments Won by Surface"
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  )
titles_surface_share_viz

save_plot_png(
  plot = titles_surface_share_viz,
  file_name = "surfacetitles.png",
  figs_dir = "../figs"
)
