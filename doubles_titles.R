library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)


player_colors <- c(
  "Federer" = "#006400",   # Green
  "Nadal" = "#D32F2F",     # Red
  "Djokovic" = "#1976D2",  # Blue
  "Alcaraz" = "#FF8C00",   # Orange
  "Sinner" = "#9C27B0"     # Pink
)

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

cleaned_doubles_data <- readRDS(file = here::here("data","processed","cleaned_doubles_data.Rds"))
head(cleaned_doubles_data)

#######################################################################################################################


fed_titles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Roger Federer" | winner2_name == "Roger Federer") & round == "F") %>%
  summarise(doubles_titles = n())
fed_titles


djokovic_titles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Novak Djokovic" | winner2_name == "Novak Djokovic") & round == "F") %>%
  summarise(doubles_titles = n())
djokovic_titles


nadal_titles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Rafael Nadal" | winner2_name == "Rafael Nadal") & round == "F") %>%
  summarise(doubles_titles = n())
nadal_titles


sinner_titles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Jannik Sinner" | winner2_name == "Jannik Sinner") & round == "F") %>%
  summarise(doubles_titles = n())
sinner_titles


alcaraz_titles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Carlos Alcaraz" | winner2_name == "Carlos Alcaraz") & round == "F") %>%
  summarise(doubles_titles = n())
alcaraz_titles


#######################################################################################################################


players_overall_titles <- bind_rows(
  fed_titles %>% mutate(player = "Roger Federer"),
  djokovic_titles %>% mutate(player = "Novak Djokovic"),
  nadal_titles %>% mutate(player = "Rafael Nadal"),
  sinner_titles %>% mutate(player = "Jannik Sinner"),
  alcaraz_titles %>% mutate(player = "Carlos Alcaraz")
)
players_overall_titles

#######################################################################################################################


doubles_titles_plot <- players_overall_titles %>%
  ggplot(aes(x = player, y = doubles_titles, fill = player)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Roger Federer"   = "#006400",
    "Rafael Nadal"    = "#D32F2F",
    "Novak Djokovic"  = "#1976D2",
    "Carlos Alcaraz"  = "#FF8C00",
    "Jannik Sinner"   = "#9C27B0"
  )) +
  geom_text(aes(label = doubles_titles), hjust = +2.5, color = "white") +
  labs(
    title = "Number of Doubles Titles per Player", x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none")
doubles_titles_plot

#######################################################################################################################


save_plot_png(plot = doubles_titles_plot, file_name = "number_doubles_titles.png", figs_dir = "figs")



