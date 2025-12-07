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


age_vs_performance <- atp_singles_data %>%
  filter(
    winner_name %in% players |
      loser_name %in% players
  ) %>%
  mutate(
    winner_result = 1,
    loser_result  = 0
  ) %>%
  select(
    player_name = winner_name,
    age = winner_age,
    result = winner_result
  ) %>%
  bind_rows(
    atp_singles_data %>%
      filter(loser_name %in% players | winner_name %in% players) %>%
      mutate(loser_result = 0) %>%
      select(
        player_name = loser_name,
        age = loser_age,
        result = loser_result
      )
  ) %>%
  filter(player_name %in% players)
age_vs_performance


age_vs_performance_summary <- age_vs_performance %>%
  group_by(player_name, age) %>%
  summarise(
    performance = mean(result),
    .groups = "drop"
  )
age_vs_performance_summary



age_vs_performance_summary$player_name <- factor(
  age_vs_performance_summary$player_name,
  levels = c(
    "Roger Federer",
    "Rafael Nadal",
    "Novak Djokovic",
    "Jannik Sinner",
    "Carlos Alcaraz"
  )
)



player_colors <- c(
  "Roger Federer"  = "#006400",  # Green
  "Rafael Nadal"   = "#D32F2F",  # Red
  "Novak Djokovic" = "#1976D2",  # Blue
  "Carlos Alcaraz" = "#FF8C00",  # Orange
  "Jannik Sinner"  = "#9C27B0"   # Pink/Purple
)



age_performance_plot <- 
  ggplot(age_vs_performance_summary,
         aes(x = age, y = performance, color = player_name)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  facet_wrap(~ player_name, scales = "free_x") +
  scale_color_manual(values = player_colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  labs(
    title = "Age vs. Performance",
    x = "Age",
    y = "Percentage of Matches Won",
    color = " "
  ) +
  theme(legend.position = "none")
age_performance_plot


save_plot_png(plot = age_performance_plot, file_name = "age_performance.png", figs_dir = "figs")




