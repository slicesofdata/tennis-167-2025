library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggrepel)


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

players <- c("Roger Federer", "Novak Djokovic", "Rafael Nadal") 


player_colors <- c(
  "Roger Federer"  = "#006400",  # Green
  "Rafael Nadal"   = "#D32F2F",  # Red
  "Novak Djokovic" = "#1976D2"  # Blue
)


#######################################################################################################################

age_last_gs <- atp_singles_data %>%
  filter(winner_name %in% players) %>%
  filter(tourney_level == "G") %>%
  filter(round == "F") %>%
  #mutate(year = year(tourney_date)) %>%
  arrange(winner_name, desc(tourney_date)) %>%
  group_by(winner_name) %>%
  slice(1) %>%  
  ungroup() %>%
  select(winner_name, tourney_date, winner_age, tourney_name)
age_last_gs


#######################################################################################################################

year_last_match <- atp_singles_data %>%
  filter(winner_name %in% players | loser_name %in% players) %>%
  #mutate(year = year(tourney_date)) %>%
  arrange(desc(tourney_date)) %>%
  pivot_longer(cols = c(winner_name, loser_name),
               names_to = "result",
               values_to = "player") %>%
  filter(player %in% players) %>%
  group_by(player) %>%
  slice(1) %>%
  ungroup() %>%
  select(player, tourney_date, tourney_name) %>%
year_last_match


year_last_match_renamed <- year_last_match %>%
  rename(
    player = player,
    year_last_match = tourney_date,
    tourney_last_match = tourney_name
  ) %>%
  select(player, year_last_match, tourney_last_match)
year_last_match_renamed



year_last_title_notGS <- atp_singles_data %>%
  filter(winner_name %in% players) %>%
  filter(tourney_level != "G") %>%
  filter(round == "F") %>%
  #mutate(year = year(tourney_date)) %>%
  arrange(winner_name, desc(tourney_date)) %>%
  group_by(winner_name) %>%
  slice(1) %>%  
  ungroup() %>%
  select(winner_name, tourney_date, winner_age, tourney_name)
year_last_title_notGS


year_last_title_notGS <- year_last_title_notGS %>%
  rename(
    player = winner_name,
    year_last_match_notGS = tourney_date,
    tourney_last_match_notGS = tourney_name
  ) %>%
  select(player, year_last_match_notGS, tourney_last_match_notGS)
year_last_title_notGS



combined_data <- age_last_gs %>%
  rename(player = winner_name,
         year_last_gs = tourney_date,
         tourney_last_gs = tourney_name) %>%
  left_join(year_last_match_renamed, by = "player") %>%
  left_join(year_last_title_notGS, by = "player") %>%
  mutate(year_retired = case_when(
    player == "Roger Federer" ~ as.Date("2021-06-28"),  
    player == "Rafael Nadal" ~ as.Date("2024-11-19"),               
    player == "Novak Djokovic" ~ NA_Date_               # still active
  ))
combined_data

#######################################################################################################################


career_timeline <- combined_data %>%
  #select(player, year_last_gs, year_last_match_notGS, year_last_match) %>%
  pivot_longer(
    cols = c(year_last_gs, year_last_match_notGS, year_last_match),
    names_to = "event",
    values_to = "date"
  ) %>%
  mutate(
    event = factor(
      event,
      levels = c("year_last_gs", "year_last_match_notGS", "year_last_match"),
      labels = c("Last GS Win", "Last Title (Non-GS)", "Retirement")
    )
  ) %>%
  filter(!(event == "Last Match" & is.na(year_retired)))
career_timeline

career_timeline_fixed <- career_timeline |>
  filter(!(player == "Novak Djokovic" & event == "Retirement"))
career_timeline_fixed

#last gs win = last grand slam each player won
#last title (non-gs) = last title each player won that wasn't a GS
#last match = last match each player played in the data set (retirement)

#######################################################################################################################

career_timeline_plot1 <- career_timeline_fixed %>%
  ggplot(aes(x = date, y = player)) +
  geom_segment(
    data = career_timeline |> 
      group_by(player) |> 
      summarize(start = min(date), end = max(date)),
    aes(x = start, xend = end, y = player, yend = player),
    linetype = "dashed", color = "grey40"
  ) +
  geom_point(aes(color = event, shape = event), size = 4) +
  scale_color_manual(values = c(
    "Last GS Win" = "#2ca02c",
    "Last Title (Non-GS)" = "#f1c40f",
    "Retirement" = "#d62728"
  )) +
  scale_shape_manual(values = c(
    "Last GS Win" = 16,
    "Last Title (Non-GS)" = 17,
    "Retirement" = 15
  )) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Big 3: End of Career Timeline",
    x = "Date",
    y = "",
    color = "",
    shape = ""
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      direction = "vertical",
      label.position = "bottom"
    ),
    shape = guide_legend(
      nrow = 1,
      direction = "vertical",
      label.position = "bottom"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    legend.key.width = unit(2, "cm"),
    legend.margin = margin(t = -15)       
  )
career_timeline_plot1



save_plot_png(plot = career_timeline_plot1, file_name = "career_timeline_plot1.png", figs_dir = "figs")



