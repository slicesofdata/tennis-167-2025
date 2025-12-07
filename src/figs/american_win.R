library(tidyverse)
library(ggplot2)
library(readr)
library(here)

atp_singles_data <- readRDS(file = here::here("data", "processed", "atp_singles.Rds"))



american_win <- atp_singles_data %>%
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
    title = "American Win %: US vs Australian Open",
    subtitle = "Visualizing home-court advantage over time",
    x = "Year",
    y = "Win Percentage (%)",
    color = "Tournament"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
american_win




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



save_plot_png(plot = american_win, file_name = "american_win.png", figs_dir = "figs")













