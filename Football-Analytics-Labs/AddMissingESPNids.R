options("install.lock" = FALSE)
install.packages('ffscrapr', repos = 'https://ffverse.r-universe.dev')

library(ffscrapr)
library(nflverse)
library(jsonlite)
library(tidyverse)
nflverse::nflverse_update(devel = TRUE)
nflverse::nflverse_sitrep()
ffscrapr::ffverse_sitrep()

# Load necessary functions
source("C:/Users/filim/Documents/R/FFAucAndDraft/HelperFunctions/load_mfl_espn_id_map.R")

#Read in Missing IDs file from Dynasty Process
old_missingplayerids <- jsonlite::fromJSON("https://raw.githubusercontent.com/dynastyprocess/data/master/files/missing_ids.json")

clean_mfl_espn_map <- load_mfl_espn_id_map()

#Next, join these fuller espn_id's to the missing ID JSON and check for contradictions
joined_new_ids <- old_missingplayerids %>%
  left_join(., y = clean_mfl_espn_map %>%
              select(mfl_id, espn_id_new = espn_id),
            by = "mfl_id")

# Test where missingplayerids file lacked our espn_ids
test_1_ids_added <- joined_new_ids %>%
  filter(is.na(espn_id) & !is.na(espn_id_new))

# Check for contradictions in espn_id; use clean_mfl_espn_map for mfl_id reference check
test_2_id_contradictions <- joined_new_ids %>%
  filter(espn_id != espn_id_new)

new_missingplayerids <- joined_new_ids %>%
  mutate(espn_id = if_else(
    is.na(espn_id) | (!is.na(espn_id) & !is.na(espn_id_new) & espn_id != espn_id_new),
    espn_id_new,  # Overwrite with espn_id_new if espn_id is NA or not equal to espn_id_new
    espn_id       # Otherwise, keep the original espn_id
  )) %>%
  select(-espn_id_new)

#convert back to json format
new_missingplayerids %>%
  toJSON(pretty = TRUE) %>%
  write(file = "C:/Users/filim/Documents/R/PullRequests/missing_ids.json")
