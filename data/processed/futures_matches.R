# Make sure output shows code but hides messages
knitr::opts_chunk$set(echo = TRUE)

# Load libraries for file paths and data handling
library(here)
library(tidyverse)
# (Optional) any custom functions can go here

# Step 1: List all ATP Futures match files in the raw data folder
files <- list.files(
  path = here("data", "raw"),
  pattern = "^atp_matches_futures_\\d{4}\\.csv$",
  full.names = TRUE
)

# Step 2: Check that the correct files were found
files

futures_matches <- files %>%
  purrr::map_dfr(~ readr::read_csv(
    .x,
    col_types = readr::cols(.default = readr::col_character()),  # force all to character
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(source_file = basename(.x))) %>%               # keep filename for debugging
  readr::type_convert()

# Step 3: Read and combine all of the Futures CSVs into one dataframe
futures_matches <- files %>%
  map_dfr(read_csv, show_col_types = FALSE)        # read each CSV and stack them together row-wise

# Step 4: Inspect the structure of the combined dataset
glimpse(futures_matches)

# Step 5: (Optional) Save the merged Futures data for future use
saveRDS(
  futures_matches,
  file = here("data", "processed", "cleaned_futures_matches.rds")
)

# Step 6: (Optional) Also write as CSV if you want to open it in Excel or another tool
write_csv(
  futures_matches,
  here("data", "processed", "cleaned_futures_matches.csv")
)
