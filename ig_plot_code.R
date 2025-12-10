library(tidyverse)
library(here)

# -----------------------------------------
# 1. Load data
# -----------------------------------------
data_merged <- source(here("data/processed/data_merged.R"))$value

# -----------------------------------------
# 2. Identify straight-set matches
# -----------------------------------------
data_merged_ss <- data_merged %>%
  mutate(
    straight_sets = grepl("^([6-7]-[0-5])( [6-7]-[0-5])*?$", score)
  )

# -----------------------------------------
# 3. Winner rank group: Top 10 vs Outside Top 50 (51+)
# -----------------------------------------
data_merged_ss_2 <- data_merged_ss %>%
  mutate(
    rank_group = case_when(
      winner_rank <= 10                      ~ "Top 10",
      winner_rank > 50 & !is.na(winner_rank) ~ "Outside Top 50",
      TRUE                                   ~ NA_character_
    )
  )

# -----------------------------------------
# 4. Opponent rank group: Top 50 vs 51+
# -----------------------------------------
data_merged_ss_3 <- data_merged_ss_2 %>%
  mutate(
    opp_rank_group = case_when(
      loser_rank <= 50                       ~ "Opponent: Top 50",
      loser_rank > 50 & !is.na(loser_rank)   ~ "Opponent: 51+",
      TRUE                                   ~ NA_character_
    )
  )

# -----------------------------------------
# 5. Compute straight-set win rates
# -----------------------------------------
rates_all <- data_merged_ss_3 %>%
  filter(
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

# -----------------------------------------
# 6. Faceted bar plot with finer y-axis ticks
# -----------------------------------------
rank_rate_facet_plot <- rates_all %>%
  ggplot(aes(x = rank_group, y = straight_set_rate, fill = rank_group)) +
  geom_col(width = 0.6, color = "black") +
  facet_wrap(~ opp_rank_group) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.10)  # 0%, 10%, 20%, ..., 100%
  ) +
  scale_fill_manual(
    values = c(
      "Top 10"          = "#1f78b4",
      "Outside Top 50"  = "#bbbbbb"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Straight-Set Win Rates\nTop 10 vs Outside Top 50 by Opponent Rank",
    x = "Winner Ranking Group",
    y = "Straight-Set Win Rate",
    fill = "Winner Rank"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

rank_rate_facet_plot

