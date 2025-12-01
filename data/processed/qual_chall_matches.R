library(tidyverse)
library(here)

# Define the path where your CSVs live
path <- here("data", "raw")

# List all CSVs that match your pattern
files <- list.files(
  path = path,
  pattern = "^atp_matches_qual_chall_\\d{4}\\.csv$",
  full.names = TRUE
)

qual_chall_matches <- files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c")))

glimpse(qual_chall_matches)

