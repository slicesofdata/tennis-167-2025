################################################################################
# Script Name: clean_mpg_data.R
# Author: Carly
# GitHub: carlybaretz
# Date Created:
#
# Purpose: This script will read raw data and process/clean it and write
# out the cleaned data.
#
################################################################################

################################################################################
# Note: When sourcing script files, if you do not want objects
# available in this script, use the source() function along with
# the local = TRUE argument. By default, source() will make
# objects available in the current environment.

################################################################################
# Load necessary libraries/source any function directories
library(dplyr)

################################################################################
# read the raw data
mpg <- readRDS("data/raw/mpg.Rds")


################################################################################
# clean the data
cleaned_mpg <-
  mpg |>
  filter(manufacturer == "audi")

################################################################################
# save the cleaned data file
cleaned_mpg |>
  saveRDS ("data/processed/cleaned_mpg.Rds")

source("src/data/clean_mpg_data.R", echo = TRUE)
file.exists("data/processed/cleaned_mpg.Rds")

################################################################################
# End of script
