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



numberofmatches_year <- ggplot(matches_per_year, aes(x = year, y = player_name, 
                                                     fill = player_name, alpha = matches)) +
  geom_tile() +
  scale_fill_manual(values = player_colors) +  
  scale_alpha_continuous(range = c(0.1, 1), guide = FALSE) +  
  labs(title = "Singles Matches Played per Year (Heatmap)",
       x = "Year",
       y = " ",
       caption = "Tile opacity reflects match volume: lighter tiles represent 
  around 25 matches, mid-tones reflect roughly 50 matches,  
  and the darkest tiles correspond to about 75 matches.") +
  scale_y_discrete(limits = rev(levels(matches_per_year$player_name))) +  
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    plot.caption = element_text(
      hjust = 0.5,      # centers the caption horizontally
      size = 10,        # adjusts font size
      face = "italic",  # makes caption italic
      margin = margin(t = 10)
    )
  )
numberofmatches_year



save_plot_png(plot = numberofmatches_year, file_name = "numberofmatches_year.png", figs_dir = "figs")






