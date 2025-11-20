library(tidyverse)
library(here)

# --- load objects ---
futures_matches   <- readRDS(here("data/processed/Joined_Futures.rds"))
joined_qual_chall <- source(here("data/processed/qual_chall_matches.R"))$value
joined_futures     <- readRDS(here("data/processed/Joined_Futures.rds"))
atp_singles       <- readRDS(here("data/processed/atp_singles.Rds"))
joined_rankings   <- readRDS(here("data/processed/joined_rankings_data.Rds"))

# --- harmonize a troublesome column across all data frames ---
fix_types <- function(df) {
  if (!"tourney_level" %in% names(df)) df$tourney_level <- NA_character_
  df %>% mutate(tourney_level = as.character(tourney_level))
}

glimpse(futures_matches)
glimpse(joined_qual_chall)
glimpse(joined_futures)
glimpse(atp_singles)
glimpse(joined_rankings)

coerce_numeric <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ suppressWarnings(as.numeric(.))))
}

coerce_stats <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ suppressWarnings(as.numeric(.))))
}

num_cols <- c(
  "draw_size", "tourney_date", "match_num",
  "winner_id", "winner_seed", "winner_ht", "winner_age", "winner_rank", "winner_rank_points",
  "loser_id", "loser_seed", "loser_ht", "loser_age", "loser_rank", "loser_rank_points",
  "best_of"
)

stats_cols <- c(
  "minutes", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon",
  "w_SvGms", "w_bpSaved", "w_bpFaced",
  "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon",
  "l_SvGms", "l_bpSaved", "l_bpFaced"
)

joined_qual_chall_clean <- joined_qual_chall %>%
  coerce_numeric(num_cols) %>%
  coerce_stats(stats_cols) %>%
  mutate(source = "challenger")

joined_futures_clean <- joined_futures %>%
  coerce_stats(stats_cols) %>%
  mutate(source = "futures")

atp_singles_clean <- atp_singles %>%
  mutate(tourney_date = as.numeric(format(tourney_date, "%Y%m%d"))) %>%
  coerce_stats(stats_cols) %>%
  mutate(source = "atp")

matches <- bind_rows(
  joined_futures_clean,
  joined_qual_chall_clean,
  atp_singles_clean
)

matches
