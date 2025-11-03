# beavis_stag_ht_chamionship.R
# Recreates the Stag hammer-throw championship plot and saves a PNG

library(tidyverse)
library(readr)
library(here)


# -------- data --------
url <- "https://raw.githubusercontent.com/slicesofdata/dataviz25/main/data/tfrrs/ht-cleaned.csv"
ht <- readr::read_csv(url, show_col_types = FALSE)

# Columns in data include (among others):
# Athlete, Year (FR/JR/SO/SR), Mark (meters), Meet, Team, Season (year), Event
# We'll use only Stag entries, championship meets, hammer throw
df <- ht %>%
  filter(Team == "Stag", Event == "HT") %>%
  filter(str_detect(Meet, regex("championship", ignore_case = TRUE))) %>%
  mutate(
    # Force consistent legend order + color mapping even if a class is missing
    Year = factor(Year, levels = c("FR","JR","SO","SR"))
  )

# -------- aesthetics --------
cols <- c(
  "FR" = "#39FF14",  # neon green (as in reference)
  "JR" = "#1f77b4",  # blue
  "SO" = "#9467bd",  # purple
  "SR" = "#d62728"   # red
)

# Reproducible jitter so overplotting is reduced without biasing y values
set.seed(42)
pt_pos <- position_jitter(width = 0.15, height = 0)

# Class-by-season means for connecting lines
df_means <- df %>%
  group_by(Season, Year) %>%
  summarise(mean_mark = mean(Mark, na.rm = TRUE), .groups = "drop")

# -------- plot --------
p <- ggplot(df, aes(x = Season, y = Mark, color = Year)) +
  geom_point(alpha = 0.45, size = 3, position = pt_pos) +
  geom_line(
    data = df_means,
    aes(y = mean_mark, group = Year),
    linewidth = 1
  ) +
  geom_point(
    data = df_means,
    aes(y = mean_mark),
    size = 3
  ) +
  scale_color_manual(values = cols, limits = c("FR","JR","SO","SR"), drop = FALSE) +
  labs(x = "Season", y = "Distance (meters)", color = "Year") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# -------- save --------
# Use your previously defined plot-saving function if present; otherwise ggsave.
# EXPECTED signature (example): save_plot_png(plot, path, width, height, dpi)
out_dir  <- here::here("figs")
out_path <- here::here("figs", "beavis_stag_ht_chamionship.png")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (exists("save_plot_png")) {
  save_plot_png(p, out_path, width = 1000, height = 690, dpi = 150)
} else if (exists("save_plot")) {
  # If your function is named `save_plot(plot, filename, width, height, dpi)`
  save_plot(p, out_path, width = 10, height = 6.9, dpi = 150)
} else {
  ggsave(filename = out_path, plot = p, width = 10, height = 6.9, dpi = 150)
}
