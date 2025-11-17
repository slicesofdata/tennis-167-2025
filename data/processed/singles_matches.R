
knitr::opts_chunk$set(echo = TRUE)

library(here)
library(tidyverse)
#functions library



files <- list.files(
  path = here("data", "raw"),
  pattern = "^atp_matches_\\d{4}\\.csv$",
  full.names = TRUE)
#pulling rankings files

files

singles_matches <- files %>%
  map_dfr(read_csv)

singles_matches

glimpse(singles_matches)
