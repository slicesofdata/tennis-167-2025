################################################################################
# Script Name: Joined Singles
# Author: Joey
# GitHub:jwilson26-stack
# Date Created:
#
# Purpose: This script ...
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
library(dplyr)
library(tidyverse)

################################################################################
# read data
singles <- readRDS(here::here("data", "processed", "atp_singles.Rds"))
singles

################################################################################
# load all data sets
joined_futures

joinedqualchall

################################################################################
# join all three large data sets
singles_futures

################################################################################
# End of script
