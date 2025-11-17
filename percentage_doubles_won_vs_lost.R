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


#######################################################################################################################

cleaned_doubles_data <- readRDS(file = here::here("data","processed","cleaned_doubles_data.Rds")) 
head(cleaned_doubles_data)

#######################################################################################################################

fed_doubles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter(winner1_name == "Roger Federer" | winner2_name == "Roger Federer" |
           loser1_name == "Roger Federer" | loser2_name == "Roger Federer") %>%
  mutate(
    won = if_else(winner1_name == "Roger Federer" | winner2_name == "Roger Federer", 1, 0)
  )
fed_doubles

fed_overall <- fed_doubles %>%
  summarise(
    matches_played = n(),
    matches_won = sum(won),
    win_percentage = (matches_won / matches_played) * 100
  )

fed_overall

#######################################################################################################################

djokovic_doubles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter(winner1_name == "Novak Djokovic" | winner2_name == "Novak Djokovic" |
           loser1_name == "Novak Djokovic" | loser2_name == "Novak Djokovic") %>%
  mutate(
    won = if_else(winner1_name == "Novak Djokovic" | winner2_name == "Novak Djokovic", 1, 0)
  )
djokovic_doubles

djokovic_overall <- djokovic_doubles %>%
  summarise(
    matches_played = n(),
    matches_won = sum(won),
    win_percentage = (matches_won / matches_played) * 100
  )

djokovic_overall

#######################################################################################################################

nadal_doubles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter(winner1_name == "Rafael Nadal" | winner2_name == "Rafael Nadal" |
           loser1_name == "Rafael Nadal" | loser2_name == "Rafael Nadal") %>%
  mutate(
    won = if_else(winner1_name == "Rafael Nadal" | winner2_name == "Rafael Nadal", 1, 0)
  )
nadal_doubles

nadal_overall <- nadal_doubles %>%
  summarise(
    matches_played = n(),
    matches_won = sum(won),
    win_percentage = (matches_won / matches_played) * 100
  )

nadal_overall

#######################################################################################################################

sinner_doubles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter(winner1_name == "Jannik Sinner" | winner2_name == "Jannik Sinner" |
           loser1_name == "Jannik Sinner" | loser2_name == "Jannik Sinner") %>%
  mutate(
    won = if_else(winner1_name == "Jannik Sinner" | winner2_name == "Jannik Sinner", 1, 0)
  )
sinner_doubles

sinner_overall <- sinner_doubles %>%
  summarise(
    matches_played = n(),
    matches_won = sum(won),
    win_percentage = (matches_won / matches_played) * 100
  )

sinner_overall

#######################################################################################################################


alcaraz_doubles <- cleaned_doubles_data %>%
  mutate(tourney_date = as.Date(tourney_date, format = "%m/%d/%Y")) %>%
  filter(winner1_name == "Carlos Alcaraz" | winner2_name == "Carlos Alcaraz" |
           loser1_name == "Carlos Alcaraz" | loser2_name == "Carlos Alcaraz") %>%
  mutate(
    won = if_else(winner1_name == "Carlos Alcaraz" | winner2_name == "Carlos Alcaraz", 1, 0)
  )
alcaraz_doubles

alcaraz_overall <- alcaraz_doubles %>%
  summarise(
    matches_played = n(),
    matches_won = sum(won),
    win_percentage = (matches_won / matches_played) * 100
  )

alcaraz_overall

#######################################################################################################################


players_overall <- bind_rows(
  fed_overall %>% mutate(player = "Roger Federer"),
  djokovic_overall %>% mutate(player = "Novak Djokovic"),
  nadal_overall %>% mutate(player = "Rafael Nadal"),
  sinner_overall %>% mutate(player = "Jannik Sinner"),
  alcaraz_overall %>% mutate(player = "Carlos Alcaraz")
)

players_stacked <- players_overall %>%
  mutate(
    loss_percentage = 100 - win_percentage
  ) %>%
  select(player, win_percentage, loss_percentage) %>%
  tidyr::pivot_longer(
    cols = c(win_percentage, loss_percentage),
    names_to = "result",
    values_to = "percentage"
  ) %>%
  mutate(
    result = if_else(result == "win_percentage", "Won", "Lost")
  )


#######################################################################################################################

doubles_plot <- 
  ggplot(players_stacked, aes(x = player, y = percentage, fill = result)) +
  geom_bar(aes(fill = interaction(player, result)), stat = "identity", position = "stack") +
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
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Percentage of Doubles Matches Won and Lost", 
       x = "", y = " ", fill = " ",
       caption = "Solid colors = Wins; Faded colors = Losses") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(
          hjust = 0.5,      # centers the caption horizontally
          size = 10,        # adjusts font size
          face = "italic",  # optional, makes it italic
          margin = margin(t = 10))) 
doubles_plot

