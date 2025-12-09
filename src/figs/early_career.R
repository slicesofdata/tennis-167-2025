


library(tidyverse)
library(ggplot2)
library(readr)
library(here)

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


atp_singles_data <- readRDS(file = here :: here("data","processed","atp_singles.Rds" ))
head(atp_singles_data)



matches <- atp_singles_data   # alias so we can call it 'matches' everywhere


# 1. Define players + first 7 seasons -------------------------------------

players <- c("Roger Federer",
             "Rafael Nadal",
             "Novak Djokovic",
             "Carlos Alcaraz",
             "Jannik Sinner")

# first_year + last_year = 7 seasons each
player_windows <- tibble::tibble(
  winner_name = players,
  first_year  = c(1998, 2001, 2003, 2018, 2018),
  last_year   = c(2004, 2007, 2009, 2024, 2024)  # 1998–2004 is 7 seasons, etc.
)


# 2. Prep match data ------------------------------------------------------

wins_raw <- matches %>%
  mutate(year = year(ymd(tourney_date))) %>%          # get year of match
  inner_join(player_windows, by = "winner_name") %>%  # add first_year / last_year
  filter(year >= first_year & year <= last_year)      # keep only first 7 seasons


# 3. Wins per year per player ---------------------------------------------

wins_yearly <- wins_raw %>%
  group_by(winner_name, year) %>%
  summarise(wins = n(), .groups = "drop")


# 4. Align by career year + cumulative wins -------------------------------

wins_cum <- wins_yearly %>%
  group_by(winner_name) %>%
  arrange(year) %>%
  mutate(
    career_year     = row_number(),
    cumulative_wins = cumsum(wins)
  ) %>%
  ungroup()


# 5. Colors ---------------------------------------------------------------

player_colors <- c(
  "Federer" = "#006400",   # Green
  "Nadal"   = "#D32F2F",   # Red
  "Djokovic"= "#1976D2",   # Blue
  "Alcaraz" = "#FF8C00",   # Orange
  "Sinner"  = "#9C27B0"    # Pink
)

player_colors_full <- setNames(
  player_colors,
  c("Roger Federer", "Rafael Nadal", "Novak Djokovic",
    "Carlos Alcaraz", "Jannik Sinner")
)

wins_cum$winner_name <- factor(
  wins_cum$winner_name,
  levels = c("Roger Federer", "Rafael Nadal", "Novak Djokovic",
             "Carlos Alcaraz", "Jannik Sinner")
)


# 6. Plot cumulative wins -------------------------------------------------

cummulative_match_wins <- ggplot(wins_cum,
       aes(x = career_year,
           y = cumulative_wins,
           color = winner_name,
           group = winner_name)) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  scale_y_continuous(breaks = seq(0, max(wins_cum$cumulative_wins), by = 50)) +
  scale_color_manual(
    values = player_colors_full,
    name = NULL,
    guide = guide_legend(nrow = 2,
                         byrow = TRUE)
  ) +
  labs(
    title = "Early-Career Trajectories:
    Cumulative Singles Match Wins (Years 1–7)",
    x = "Career Year (aligned by debut season)",
    y = "Cumulative Match Wins"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10)
  )
cummulative_match_wins

save_plot_png(plot = cummulative_match_wins, file_name = "cummulative_match_wins.png", figs_dir = "figs")





wins_age <- wins_raw %>%
  filter(!is.na(winner_age)) %>%
  select(winner_name, winner_age)



wins_by_age <- wins_age %>%
  mutate(winner_age = floor(winner_age)) %>%
  group_by(winner_name, winner_age) %>%
  summarise(n_wins = n(), .groups = "drop")

wins_start <- ggplot(wins_by_age,
                     aes(x = winner_age,
                         y = n_wins,
                         color = winner_name,
                         group = winner_name)) +
  geom_line(linewidth = 1.2, alpha = 0.6) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_manual(
    values = player_colors_full,
    breaks = c("Roger Federer",
               "Rafael Nadal",
               "Novak Djokovic",
               "Carlos Alcaraz",
               "Jannik Sinner"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(17, 21), breaks = 17:21) +
  labs(
    title = "Match Wins at Each Age (Early-Career)",
    x = "Age",
    y = "Number of Match Wins"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5,
      vjust = 1.5,
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.title.position = "plot"
  )

wins_start


save_plot_png(plot = wins_start, file_name = "wins_start.png", figs_dir = "figs")

