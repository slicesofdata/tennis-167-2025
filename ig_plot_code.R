library(tidyverse)
library(here)

data_merged <- source(here("data/processed/data_merged.R"))$value

data_merged_ss <- data_merged %>%
  mutate(
    straight_sets = grepl("^([6-7]-[0-5])( [6-7]-[0-5])*?$", score)
  )

data_merged_ss_2 <- data_merged_ss %>%
  mutate(
    rank_group = case_when(
      winner_rank <= 100                       ~ "Top 100",
      winner_rank > 100 & !is.na(winner_rank)  ~ "101+",
      TRUE                                     ~ NA_character_
    )
  )

data_merged_ss_3 <- data_merged_ss_2 %>%
  mutate(
    opp_rank_group = case_when(
      loser_rank <= 100                        ~ "Opponent: Top 100",
      loser_rank > 100 & !is.na(loser_rank)    ~ "Opponent: 101+",
      TRUE                                     ~ NA_character_
    )
  )
rates_all <- data_merged_ss_3 %>%
  filter(
    winner_age >= 25,             # keep if you still want "older winners"
    !is.na(rank_group),
    !is.na(opp_rank_group)
  ) %>%
  group_by(opp_rank_group, rank_group) %>%
  summarise(
    straight_set_rate = mean(straight_sets, na.rm = TRUE),
    matches = n(),
    .groups = "drop"
  )

rates_all

# Faceted plot:
# - one facet for Opponent: Top 100
# - one facet for Opponent: 101+
# - within each, bars for winner Top 100 vs 101+
rank_rate_facet_plot <- rates_all %>%
  ggplot(aes(x = rank_group, y = straight_set_rate, fill = rank_group)) +
  geom_col(width = 0.6, color = "black") +
  facet_wrap(~ opp_rank_group) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("Top 100" = "#1f78b4", "101+" = "#bbbbbb")) +
  theme_minimal() +
  labs(
    title = "Straight-Set Win Rates (Age ≥ 25)\nby Winner & Opponent Ranking Group",
    x = "Winner Ranking Group",
    y = "Straight-Set Win Rate",
    fill = "Winner Rank"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

rank_rate_facet_plot
