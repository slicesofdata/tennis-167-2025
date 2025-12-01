################################################################################
# Script Name: Joined Qual Chall
# Author: Joey
# GitHub: jwilson26-stack
# Date Created: 9/20/25
#
# Purpose: This script is to join Qual Chall data into one table
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
library(here)
library(tidyverse)
library(readr)
library(purrr)

################################################################################
# Pull Qual Chall docs into one assigned table
QualsFiles <- list.files(
  path=here("data", "raw"),
  pattern = "^atp_matches_qual_chall_\\d+\\.csv$",
  full.names = TRUE
)
QualsFiles

################################################################################
# Create data table
all_quals <- QualsFiles %>%
  map_dfr(read_csv)
all_quals

################################################################################
# 
saveRDS(all_quals, file = here::here("data", "processed", "JoinedQualChall.rds"))

################################################################################
# End of script.
