library(tidyverse)
library(here)

# --- load existing processed objects ---
futures_matches   <- readRDS(here("data/processed/joined_futures.Rds"))
joined_qual_chall <- readRDS(here("data/processed/joinedqualchall.Rds"))
atp_singles       <- readRDS(here("data/processed/atp_singles.Rds"))
joined_rankings   <- readRDS(here("data/processed/joined_rankings.Rds"))

# --- normalize tourney_date everywhere (IMPORTANT!) ---
futures_matches$tourney_date   <- as.numeric(futures_matches$tourney_date)
joined_qual_chall$tourney_date <- as.numeric(joined_qual_chall$tourney_date)
atp_singles$tourney_date       <- as.numeric(format(atp_singles$tourney_date, "%Y%m%d"))

# --- harmonize a troublesome column across all data frames ---
fix_types <- function(df) {
  if (!"tourney_level" %in% names(df)) df$tourney_level <- NA_character_
  df %>% mutate(tourney_level = as.character(tourney_level))
}

# --- package everything into a single list before binding ---
dfs <- list(
  futures_matches   = futures_matches   %>% fix_types(),
  joined_qual_chall = joined_qual_chall %>% fix_types(),
  atp_singles       = atp_singles       %>% fix_types()
)

# --- bind into a long unified frame ---
all_matches <- bind_rows(dfs, .id = "source")

# OPTIONAL: join rankings if needed later
# all_matches <- left_join(all_matches, joined_rankings, by = "player_id")

all_matches
