library(tidyverse)
library(here)

# --- load objects ---
futures_matches   <- source(here("data/processed/futures_matches.R"))$value
joined_qual_chall <- source(here("data/processed/qual_chall_matches.R"))$value
joinedfutures     <- source(here("data/processed/joinedfutures.R"))$value
atp_singles       <- readRDS(here("data/processed/atp_singles.Rds"))
joined_rankings   <- readRDS(here("data/processed/joined_rankings_data.Rds"))

# --- harmonize a troublesome column across all data frames ---
fix_types <- function(df) {
  if (!"tourney_level" %in% names(df)) df$tourney_level <- NA_character_
  df %>% mutate(tourney_level = as.character(tourney_level))
}

dfs <- list(
  futures_matches   = futures_matches   %>% fix_types(),
  joined_qual_chall = joined_qual_chall %>% fix_types(),
  joinedfutures     = joinedfutures     %>% fix_types(),
  atp_singles       = atp_singles       %>% fix_types()
)

# --- now it will bind cleanly ---
all_matches <- bind_rows(dfs, .id = "source")

# (optional) join rankings if needed (adjust keys as appropriate)
# all_matches <- left_join(all_matches, joined_rankings, by = "player_id")
