library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)
library(scales)

save_plot_png <- function(
    plot,
    file_name,
    figs_dir,
    units = "px",
    width = 1600,
    height = 1100,
    dpi = 300) {

  file_path <- here::here("figs", file_name)

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


player_colors <- c(
  "Federer" = "#006400",   # Green
  "Nadal" = "#D32F2F",     # Red
  "Djokovic" = "#1976D2",  # Blue
  "Alcaraz" = "#FF8C00",   # Orange
  "Sinner" = "#9C27B0"     # Pink
)


#######################################################################################################################

cleaned_rankings_data <- readRDS(file = here::here("data","processed","cleaned_rankings_data.rds"))
head(cleaned_rankings_data)

atp_singles_data <- readRDS(file = here::here("data","processed","atp_singles.Rds"))
head(atp_singles_data)

#######################################################################################################################

filtered_atp_singles <- atp_singles_data %>%
  filter(
    winner_name %in% c("Roger Federer", "Novak Djokovic", "Rafael Nadal")
  ) %>%
  filter (tourney_level == "G") %>%
  filter(round == "F") %>%
  select(tourney_name, tourney_date, winner_name, winner_rank)
filtered_atp_singles

#######################################################################################################################

cleaneddata_federer <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Federer") %>%
  filter(name_first == "Roger")
cleaneddata_federer

#######################################################################################################################

cleaneddata_nadal <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Nadal") %>%
  filter(name_first == "Rafael")
cleaneddata_nadal

#######################################################################################################################

cleaneddata_djokovic <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Djokovic") %>%
  filter(name_first == "Novak")
cleaneddata_djokovic

#######################################################################################################################

#shows the big three ranking during this specific time frame represented by different colors
big3_ranking_timeline <-
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_smooth(data = cleaneddata_federer, aes(color = "Roger Federer"), se = FALSE, method = "loess", linewidth = 1) +
  geom_smooth(data = cleaneddata_nadal, aes(color = "Rafael Nadal"), se = FALSE, method = "loess", linewidth = 1) +
  geom_smooth(data = cleaneddata_djokovic, aes(color = "Novak Djokovic"), se = FALSE, method = "loess", linewidth = 1) +
  geom_point(data = filtered_atp_singles, aes(x = tourney_date, y = winner_rank, color = winner_name),
             size = 1.5, stroke = 0.4, shape = 21,
             position = position_jitter()) +
  labs(x = "Date", y = "Ranking (1 = Highest)", color = "", shape = "Grand Slam Wins",
       title = "ATP Singles Ranking: Big 3",
       caption = "Grand Slam wins are marked by circles on the plot.") +
  scale_color_manual(values = c("Roger Federer" = "#006400",
                                "Rafael Nadal" = "#D32F2F",
                                "Novak Djokovic" = "#1976D2")) +
  scale_y_reverse(limits = c(6,1),
                  breaks = 6:1,
                  labels = scales::number) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_classic() +
  theme(
    legend.position = "bottom",   # Place legend at the bottom
    legend.box = "vertical",      # Ensure the legend is in vertical box format
    plot.caption = element_text(hjust = 0.5, vjust = 0.5),  # Horizontally center the caption
    plot.margin = margin(t = 10, b = 40, l = 10, r = 10)  # Adjust margins if necessary
  )
big3_ranking_timeline


save_plot_png(plot = big3_ranking_timeline, file_name = "big3_ranking_timeline_final.png", figs_dir = "figs")

#maybe add a fourth line that shows the total (see domination?)

#######################################################################################################################

cleaneddata_sinner <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Sinner") %>%
  filter(name_first == "Jannik") %>%
  mutate(age = as.numeric((ranking_date - dob) / 365.25))
cleaneddata_sinner

#######################################################################################################################

cleaneddata_alcaraz <- cleaned_rankings_data %>%
  mutate(dob = as.Date(as.character(dob), format = "%Y%m%d")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  mutate(height = as.numeric(height)) %>%
  filter(name_last == "Alcaraz") %>%
  filter(name_first == "Carlos") %>%
  mutate(age = as.numeric((ranking_date - dob) / 365.25))
cleaneddata_alcaraz

#######################################################################################################################


#grand slam data for the New 2

filtered_atp_singles_new2 <- atp_singles_data %>%
  filter(
    winner_name %in% c("Jannik Sinner", "Carlos Alcaraz")
  ) %>%
  filter (tourney_level == "G") %>%
  filter(round == "F") %>%
  select(tourney_name, tourney_date, winner_name, winner_rank)
filtered_atp_singles_new2


#this is a graph for sinner and alcaraz singles ranking from 2022-current (top 10 only)
new2_ranking_timeline1 <-
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_smooth(data = cleaneddata_sinner, aes(color = "Jannik Sinner"), se = FALSE, method = "loess", linewidth = 1) +
  geom_smooth(data = cleaneddata_alcaraz, aes(color = "Carlos Alcaraz"), se = FALSE, method = "loess", linewidth = 1) +
  geom_point(data = filtered_atp_singles_new2, aes(x = tourney_date, y = winner_rank, color = winner_name),
             size = 2.5, alpha = 0.6) +

  labs(x = "Date", y = "Ranking (1 = Highest)", color = " ",
       title = "ATP Singles Ranking: Alcaraz vs Sinner (2022-2025)",

       caption = "Grand Slam wins are marked by circles on the plot.") +
  scale_color_manual(values = c("Jannik Sinner" = "#9C27B0",
                                "Carlos Alcaraz" = "#FF8C00")) +
  scale_y_reverse(limits = c(10,1),
                  breaks = 10:1,
                  labels = scales::number_format(accuracy = 1)) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_classic() +
  theme(
    legend.position = "bottom",   # Place legend at the bottom
    legend.box = "vertical",      # Ensure the legend is in vertical box format
    plot.caption = element_text(hjust = 0.5, vjust = 0.5),  # Horizontally center the caption
    plot.margin = margin(t = 10, b = 40, l = 10, r = 10)  # Adjust margins if necessary
  )
new2_ranking_timeline1

save_plot_png(plot = new2_ranking_timeline1, file_name = "new2_ranking_timeline_final.png", figs_dir = "figs")


###this is the same graph as above, just using geom_line instead of geom_smooth

new2_ranking_timeline <-
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_line(data = cleaneddata_sinner, aes(color = "Sinner")) +
  geom_line(data = cleaneddata_alcaraz, aes(color = "Alcaraz")) +
  labs(x = "Date", y = "Ranking (1 = Highest)", color = "Player", title = "ATP Singles Ranking: Alcaraz vs Sinner (2022-2025)") +
  scale_color_manual(values = c("Sinner" = "#9C27B0",
                                "Alcaraz" = "#FF8C00")) +
  scale_x_date(limits = as.Date(c("2022-01-01", Sys.Date()))) +
  scale_y_reverse(limits = c(10,1),
                  breaks = 10:1,
                  labels = scales::number_format(accuracy = 1)) +
  annotate("point", x = as.Date("2024-01-28"), y = 4, size = 1) +
  annotate("text", x = as.Date("2024-01-28"), y = 4.25,
           label = "Australian Open",
           size = 3) + #this adds a point and text to show the sinner's AO win
  annotate("point", x = as.Date("2024-09-6"), y = 1, size = 1) +
  annotate("text", x = as.Date("2024-09-6"), y = 1.25,
           label = "US Open",
           size = 3) +
  annotate("point", x = as.Date("2022-09-11"), y = 1, size = 1) +
  annotate("text", x = as.Date("2022-09-11"), y = 1.25,
           label = "US Open",
           size = 3) +
  annotate("point", x = as.Date("2023-09-16"), y = 2, size = 1) +
  annotate("text", x = as.Date("2023-09-16"), y = 2.25,
           label = "Wimbledon",
           size = 3) +
  annotate("point", x = as.Date("2024-06-09"), y = 2, size = 1) +
  annotate("text", x = as.Date("2024-06-09"), y = 2.25,
           label = "French Open",
           size = 3) +
  annotate("point", x = as.Date("2024-07-14"), y = 3, size = 1) +
  annotate("text", x = as.Date("2024-07-14"), y = 3.25,
           label = "Wimbledon",
           size = 3) +
  theme_classic()
new2_ranking_timeline

#######################################################################################################################


##this graph is going to show the week spent at number 1 for our 5 players of interest


#calculate the number of weeks at number 1
week_number1 <- cleaned_rankings_data %>%
  filter(rank == "1") %>%
  filter(name_last %in% c("Sinner", "Alcaraz", "Djokovic", "Nadal", "Federer")) %>%
  filter(name_first %in% c("Jannik", "Carlos", "Novak", "Rafael", "Roger")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  arrange(player_id, ranking_date) %>%
  mutate(week = floor_date(ranking_date, "week"))
week_number1

weeks_at_rank_1 <- week_number1 %>%
  group_by(name_last) %>%
  summarise(
    total_weeks_at_rank_1 = n_distinct(week)  # Count distinct weeks
  ) %>%
  arrange(desc(total_weeks_at_rank_1)) %>%
  mutate(name_last = factor(name_last, levels = name_last))
weeks_at_rank_1


#calculate the total number of weeks
total_weeks_ranked <- cleaned_rankings_data %>%
  filter(name_last %in% c("Sinner", "Alcaraz", "Djokovic", "Nadal", "Federer")) %>%
  filter(name_first %in% c("Jannik", "Carlos", "Novak", "Rafael", "Roger")) %>%
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d")) %>%
  arrange(player_id, ranking_date) %>%
  mutate(week = floor_date(ranking_date, "week")) %>%
  group_by(name_last) %>%
  summarise(total_weeks_ranked = n_distinct(week))
total_weeks_ranked

weeks_summary <- weeks_at_rank_1 %>%
  left_join(total_weeks_ranked, by = "name_last") %>%
  # Calculate percentage of weeks at rank 1
  mutate(percentage_weeks_at_rank_1 = (total_weeks_at_rank_1 / total_weeks_ranked) * 100) %>%
  arrange(desc(percentage_weeks_at_rank_1)) %>%
  mutate(name_last = factor(name_last, levels = name_last))
weeks_summary



#graph with number of weeks
weeks_rank1_graph_1 <- weeks_at_rank_1 %>%
  ggplot(mapping = aes(x = name_last, y = total_weeks_at_rank_1, fill = name_last)) +
  geom_col() +
  geom_text(aes(label = round(total_weeks_at_rank_1, 1)),
            vjust = -0.5,   # Adjust this to move the text above the bar
            size = 3) +
  labs(x = " ", y = "Weeks", title = "Total Number of Weeks Spent at #1 Ranking",
       fill = "Player") +
  scale_fill_manual(values = player_colors) +
  theme_minimal() +
  theme(legend.position = "none")
weeks_rank1_graph_1



#graph with percentage of weeks at #1
weeks_rank1_graph_2 <- weeks_summary %>%
  ggplot(mapping = aes(x = name_last, y = percentage_weeks_at_rank_1, fill = name_last)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100)) +
  geom_text(aes(label = percent(percentage_weeks_at_rank_1 / 100, accuracy = 0.1)),
            vjust = -0.5,   # Adjust this to move the text above the bar
            size = 3) +
  labs(x = "", y = " ", fill = "Player",
       title = "Percentage of Weeks Spent at #1 Ranking") +
  scale_fill_manual(values = player_colors) +
  theme_minimal() +
  theme(legend.position = "none")
weeks_rank1_graph_2

save_plot_png(plot = weeks_rank1_graph_2, file_name = "number1_percentage.png", figs_dir = "figs")

#######################################################################################################################
