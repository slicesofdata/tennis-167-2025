library(tidyverse)
library(here)
library(ggforce)

# ---------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------
data_merged <- source(here("data/processed/data_merged.R"))$value

# ---------------------------------------------------------
# 2. Keep TRUE upsets and compute upset magnitude
# ---------------------------------------------------------
upsets <- data_merged %>%
  filter(
    !is.na(winner_rank),
    !is.na(loser_rank),
    winner_rank > loser_rank      # winner is worse-ranked than loser = upset
  ) %>%
  mutate(
    upset_gap = winner_rank - loser_rank
  )

# ---------------------------------------------------------
# 3. Trim extreme upsets so the plot is readable
# ---------------------------------------------------------
max_gap <- 100   # change to 75 if you want an even tighter view

upsets_trim <- upsets %>%
  filter(upset_gap <= max_gap) %>%
  mutate(
    winner_rank_group = case_when(
      winner_rank <= 50              ~ "Winner: Top 50",
      winner_rank <= 100             ~ "Winner: 51–100",
      winner_rank > 100              ~ "Winner: 101+",
      TRUE                           ~ NA_character_
    )
  ) %>%
  filter(!is.na(winner_rank_group)) %>%
  mutate(
    winner_rank_group = factor(
      winner_rank_group,
      levels = c("Winner: Top 50", "Winner: 51–100", "Winner: 101+")
    )
  )

# ---------------------------------------------------------
# 4. Summary stats for medians + IQR
# ---------------------------------------------------------
upset_summaries <- upsets_trim %>%
  group_by(winner_rank_group) %>%
  summarise(
    q1  = quantile(upset_gap, 0.25),
    med = median(upset_gap),
    q3  = quantile(upset_gap, 0.75),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 5. Violin + median + IQR, axis ticks every 5 units
# ---------------------------------------------------------
upset_sina_plot <- ggplot(
  upsets_trim,
  aes(x = winner_rank_group, y = upset_gap, fill = winner_rank_group)
) +
  # distribution shape (violin)
  geom_violin(
    alpha = 0.6,
    color = "black",
    trim  = FALSE,
    linewidth = 0.4
  ) +

  # IQR line
  geom_segment(
    data = upset_summaries,
    aes(
      x = winner_rank_group,
      xend = winner_rank_group,
      y = q1,
      yend = q3
    ),
    color = "black",
    linewidth = 1
  ) +

  # median point
  geom_point(
    data = upset_summaries,
    aes(x = winner_rank_group, y = med),
    color = "black",
    size  = 2
  ) +

  # flip for horizontal scale
  coord_flip() +

  # *** axis ticks every 5 units ***
  scale_y_continuous(breaks = seq(0, max_gap, by = 5)) +

  # color palette
  scale_fill_manual(
    values = c(
      "Winner: Top 50" = "#1f78b4",
      "Winner: 51–100" = "#33a02c",
      "Winner: 101+"   = "#fb9a99"
    )
  ) +

  labs(
    title    = "Upset Magnitudes in ATP Matches",
    subtitle = paste0(
      "Rank-gap (winner rank – loser rank) for true upsets by winner's ranking group\n",
      "Showing only upsets with gap ≤ ", max_gap,
      " (typical range); extreme outliers omitted for clarity."
    ),
    x    = "",
    y    = "Upset Magnitude (Winner Rank – Loser Rank)",
    fill = "Winner Rank Group"
  ) +

  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

# ---------------------------------------------------------
# 6. Display the plot
# ---------------------------------------------------------
upset_sina_plot

