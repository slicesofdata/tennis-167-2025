################################################################################
# Script Name: Cleaned Futures Data
# Author: Joey
# GitHub: jwilson26-stack
# Date Created: 9/20/25
#
# Purpose: This script is to join the appropriate data files into a single table for us to
# work with in the future.
#
################################################################################

################################################################################
# Note: When sourcing script files, if you do not want objects
# available in this script, use the source() function along with
# the local = TRUE argument. By default, source() will make
# objects available in the current environment.

################################################################################
# Load necessary libraries/source any function directories
# Example:
#R.utils::sourceDirectory(here::here("src", "functions"))
source(here::here("src", "functions", "load-libraries.R"))

################################################################################
library(here)
library(tidyverse)
library(readr)
library(purrr)


################################################################################
# pulling all of futures files
FuturesFiles <- list.files(
  path=here("data", "raw"),
  pattern = "^atp_matches_futures_\\d+\\.csv$",
  full.names = TRUE
)
FuturesFiles

################################################################################
# Create Data Table; Files would not join at first, as some columns disagreed.
All_Futures <- FuturesFiles %>%
  map_dfr(~ read_csv(.x,
                     col_types = cols(
                       .default = col_guess(),
                       tourney_level = col_character()
                     )))

All_Futures

################################################################################
# End of script, Saving RDS to data processed folder
saveRDS(All_Futures, file = here::here("data", "processed", "joined_futures.rds"))

