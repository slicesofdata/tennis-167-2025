library(tidyverse)
library(ggplot2)
library(readr)
library(here)

atp_singles_data <- readRDS(file = here::here("data", "processed", "atp_singles.Rds"))
selected_countries <- c("USA", "ESP", "FRA", "ITA", "GER")


wins_selected <- atp_singles_data %>%
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
wins_selected


graph2 <- ggplot(wins_selected, aes(x = year, y = wins, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(
    title = "Match Wins by Country Over Time ",
    subtitle = "USA, Spain, France, Italy, and Germany (1968–2025)",
    x = "Year",
    y = "Number of Match Wins",
    color = "Country"
  ) +
  scale_x_continuous(
    breaks = seq(1970, max(wins_selected$year), by = 10)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1750, by = 250)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
graph2


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



save_plot_png(plot = graph2, file_name = "leading_country.png", figs_dir = "figs")
#












