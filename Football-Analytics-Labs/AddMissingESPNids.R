options("install.lock" = FALSE)
install.packages('ffscrapr', repos = 'https://ffverse.r-universe.dev')
install.packages("nflverse")
install.packages("jsonlite")

library(ffscrapr)
library(nflverse)
library(jsonlite)
library(tidyverse)
nflverse::nflverse_update(devel = TRUE)

#Read in Missing IDs file from Dynasty Process
old_missingplayerids <- jsonlite::fromJSON("https://raw.githubusercontent.com/dynastyprocess/data/master/files/missing_ids.json")

#First populate full MFL player list
mfl_names_list <- ffscrapr::mfl_players() %>%
  select(player_id,
         player_name,
         pos,
         team,
         espn_id) %>%
  rename(mfl_id = player_id,
         mfl_pos = pos,
         mfl_name_raw = player_name,
         mfl_team_raw = team) %>%
  mutate(player_name = nflreadr::clean_player_names(mfl_name_raw),
         team = nflreadr::clean_team_abbrs(mfl_team_raw),
         #Change mfl_id type to match original JSON
         mfl_id = as.integer(mfl_id),
         #Change espn_id type to match original JSON
         espn_id = as.integer(espn_id)) %>%
  # Filter out all MFL team position entries
  filter(mfl_id > 999)

# Clean ESPN player list for join
espn_names_list <- ffscrapr::espn_players() %>%
  rename(espn_id = player_id,
         espn_pos = pos,
         espn_name_raw = player_name,
         espn_team_raw = team) %>%
  mutate(player_name = nflreadr::clean_player_names(espn_name_raw),
         team = nflreadr::clean_team_abbrs(espn_team_raw))

# Find errors in mfl_players() espn_id by mapping mfl_players() to espn_players() by joining name and team
# Where test_1 entries are from duplicates above, we need to filter out wrong duplicate
# Where they are not duplicates, we need to overwrite incorrect data

joined_mfl_espn_players_raw <- mfl_names_list %>%
  full_join(., y = espn_names_list,
            by = c("player_name",
                   "team"))

# Test for contradictions in joined list
test_1_contradictions <- joined_mfl_espn_players_raw %>%
  filter(espn_id.x != espn_id.y)
view(test_1_contradictions)

# Test for duplicates in joined list to show which contradictions are from over-joined duplicates
test_2_duplicates <- joined_mfl_espn_players_raw %>%
  group_by(player_name, team) %>%
  filter(n() > 1) %>%
  arrange(player_name, team)
view(test_2_duplicates)

joined_mfl_espn_players_cleaned <- joined_mfl_espn_players_raw %>%
  mutate(espn_id.x = if_else(
    player_name %in% c("Nick Williams", "Randy Gregory", "Ramiz Ahmed") | is.na(espn_id.x),
    espn_id.y,  # Use espn_players() ID to correct errors and overwrite missing values
    espn_id.x   # Otherwise, keep mfl_players() espn_id value
  )) %>%
  #Filter out remaining contradictions from duplicates or team mismatch
  filter(espn_id.x == espn_id.y | is.na(espn_id.x) | is.na(espn_id.y)) %>%
  mutate(espn_id_new = espn_id.x) %>%
  select(-espn_id.x, -espn_id.y)

view(joined_mfl_espn_players_cleaned)

#Next, join these new espn_id's to the missing ID JSON and check for contradictions
complete_join <- old_missingplayerids %>%
  left_join(., y = joined_mfl_espn_players_cleaned %>%
              select(mfl_id, espn_id_new),
            by = "mfl_id")

#Check for contradictions in espn_id; use joined_mfl_espn_players_cleaned for mfl_id reference check
test_1_contradictions_final <- complete_join %>%
  filter(espn_id != espn_id_new)

new_missingplayerids <- complete_join %>%
  mutate(espn_id = if_else(
    is.na(espn_id) | espn_id != espn_id_new,
    espn_id_new,  # Overwrite with espn_id_new if espn_id is NA or not equal to espn_id_new
    espn_id       # Otherwise, keep the original espn_id
  ))

#convert back to json format
new_missingplayerids %>%
  toJSON(pretty = TRUE) %>%
  write(file = "C:/Users/filim/Documents/R/PullRequests/missing_ids.json")