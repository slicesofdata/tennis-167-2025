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

fed_win_percentage <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Roger Federer" | winner2_name == "Roger Federer" |
          loser1_name == "Roger Federer" | loser2_name == "Roger Federer")) %>%
  mutate(fed_win = case_when(
    winner1_name == "Roger Federer" | winner2_name == "Roger Federer" ~ 1,  # Win
    loser1_name == "Roger Federer" | loser2_name == "Roger Federer" ~ 0   # Loss
  )) %>%
  summarise(
    total_matches = n(),
    total_wins = sum(fed_win),
    win_percentage = (total_wins / total_matches) * 100
  )
fed_win_percentage
  
#######################################################################################################################

djokovic_win_percentage <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Novak Djokovic" | winner2_name == "Novak Djokovic" |
            loser1_name == "Novak Djokovic" | loser2_name == "Novak Djokovic")) %>%
  mutate(novak_win = case_when(
    winner1_name == "Novak Djokovic" | winner2_name == "Novak Djokovic" ~ 1,  # Win
    loser1_name == "Novak Djokovic" | loser2_name == "Novak Djokovic" ~ 0   # Loss
  )) %>%
  summarise(
    total_matches = n(),
    total_wins = sum(novak_win),
    win_percentage = (total_wins / total_matches) * 100
  )
djokovic_win_percentage
  
#######################################################################################################################
  
nadal_win_percentage <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Rafael Nadal" | winner2_name == "Rafael Nadal" |
            loser1_name == "Rafael Nadal" | loser2_name == "Rafael Nadal")) %>%
  mutate(nadal_win = case_when(
    winner1_name == "Rafael Nadal" | winner2_name == "Rafael Nadal" ~ 1,  # Win
    loser1_name == "Rafael Nadal" | loser2_name == "Rafael Nadal" ~ 0   # Loss
  )) %>%
  summarise(
    total_matches = n(),
    total_wins = sum(nadal_win),
    win_percentage = (total_wins / total_matches) * 100
  )
nadal_win_percentage

#######################################################################################################################
  
sinner_win_percentage <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Jannik Sinner" | winner2_name == "Jannik Sinner" |
            loser1_name == "Jannik Sinner" | loser2_name == "Jannik Sinner")) %>%
  mutate(sinner_win = case_when(
    winner1_name == "Jannik Sinner" | winner2_name == "Jannik Sinner" ~ 1,  # Win
    loser1_name == "Jannik Sinner" | loser2_name == "Jannik Sinner" ~ 0   # Loss
  )) %>%
  summarise(
    total_matches = n(),
    total_wins = sum(sinner_win),
    win_percentage = (total_wins / total_matches) * 100
  )
sinner_win_percentage

#######################################################################################################################

alcaraz_win_percentage <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter((winner1_name == "Carlos Alcaraz" | winner2_name == "Carlos Alcaraz" |
            loser1_name == "Carlos Alcaraz" | loser2_name == "Carlos Alcaraz")) %>%
  mutate(alcaraz_win = case_when(
    winner1_name == "Carlos Alcaraz" | winner2_name == "Carlos Alcaraz" ~ 1,  # Win
    loser1_name == "Carlos Alcaraz" | loser2_name == "Carlos Alcaraz" ~ 0   # Loss
  )) %>%
  summarise(
    total_matches = n(),
    total_wins = sum(alcaraz_win),
    win_percentage = (total_wins / total_matches) * 100
  )
alcaraz_win_percentage

#######################################################################################################################

players_overall_win_loss <- bind_rows(
  fed_win_percentage %>% mutate(player = "Roger Federer"),
  djokovic_win_percentage %>% mutate(player = "Novak Djokovic"),
  nadal_win_percentage %>% mutate(player = "Rafael Nadal"),
  sinner_win_percentage %>% mutate(player = "Jannik Sinner"),
  alcaraz_win_percentage %>% mutate(player = "Carlos Alcaraz")
)
players_overall_win_loss


#######################################################################################################################

doubles_win_loss <- players_overall_win_loss %>%
  mutate(total_losses = total_matches - total_wins) %>%
  gather(key = "result", value = "count", total_wins, total_losses) %>%
  ggplot(mapping = aes(x = player, y = count, fill = result)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Roger Federer.Won"  = "#006400",   
      "Roger Federer.Lost" = scales::alpha("#006400", 0.5),
      "Rafael Nadal.Won"   = "#D32F2F",
      "Rafael Nadal.Lost"  = scales::alpha("#D32F2F", 0.5),
      "Novak Djokovic.Won" = "#1976D2",
      "Novak Djokovic.Lost"= scales::alpha("#1976D2", 0.5),
      "Carlos Alcaraz.Won" = "#FF8C00",
      "Carlos Alcaraz.Lost"= scales::alpha("#FF8C00", 0.5),
      "Jannik Sinner.Won"  = "#9C27B0",
      "Jannik Sinner.Lost" = scales::alpha("#9C27B0", 0.5)
    )
  ) +
  labs(title = "Total Wins and Losses for Each Player", 
       x = " ", 
       y = "Number of Matches", 
       fill = "Result") +
  theme_minimal()
doubles_win_loss





  