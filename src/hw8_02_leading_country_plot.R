# R/02_leading_country_plot.R

library(tidyverse)   # includes ggplot2
library(here)

# Read in the summarized data created by the data script
wins_selected <- readRDS(
  here::here("data", "processed", "wins_selected.rds")
)

# Create the plot (this is your original plot)
graph2 <- ggplot(wins_selected, aes(x = year, y = wins, color = country)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  labs(
    title = "Match Wins by Country Over Time",
    subtitle = "USA, Spain, France, Italy, and Germany (1968–2025)",
    x = "Year",
    y = "Number of Match Wins",
    color = "Country"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Save the plot as a PNG in figs/
ggplot2::ggsave(
  filename = here::here("figs", "leading_country.png"),
  plot = graph2,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)
