#Script used to identify necessary ADL/FAFL position changes within MFL to comply with bylaws

install.packages("ffscrapr", repos = c("https://ffverse.r-universe.dev", getOption("repos")))
library(nflverse)
library(ffespn)
library(ffscrapr)
library(tidyverse)
nflverse::nflverse_update(devel = TRUE)
nflverse::nflverse_sitrep()
ffscrapr::ffverse_sitrep()

ADL24 <- mfl_connect(season = 2024, league_id = 60206, user_agent = "mikeyfili", rate_limit_number = 3, rate_limit_seconds = 6)
FAFL24 <- mfl_connect(season = 2024, league_id = 22686, user_agent = "mikeyfili", rate_limit_number = 3, rate_limit_seconds = 6)

#Create function to generate all players and current position changes in MFL league
get_league_players <- function(conn) {
  players <- ffscrapr::mfl_players(conn) %>%
    rename(mfl_id = player_id,
           league_pos = pos,
           mfl_player_name = player_name,
           mfl_team = team) %>%
    mutate(mfl_clean_player_name = nflreadr::clean_player_names(mfl_player_name),
           clean_team_name = nflreadr::clean_team_abbrs(mfl_team)) %>%
    # Join default MFL player list to populate default MFL positions
    left_join(., y = ffscrapr::mfl_players() %>%
                rename(mfl_id = player_id,
                       mfl_default_pos = pos) %>%
                select(c("mfl_id", "mfl_default_pos")),
              by = c("mfl_id")) %>%
    relocate(mfl_default_pos, .after = league_pos) %>%
    # Join with nflreadr player IDs to update espn_id
    left_join(., y = nflreadr::load_ff_playerids() %>%
                select(mfl_id, espn_id),
              by = "mfl_id") %>%
    mutate(espn_id = as.integer(coalesce(espn_id.x, espn_id.y))) %>%
    select(-espn_id.x, -espn_id.y)  # Clean up temporary columns
  
  # Build df showing current league position changes. Should match MFL URL https://www46.myfantasyleague.com/2024/csetup?L=60206&C=CHGPOS
  curr_pos_changes <- players %>%
    filter(league_pos != mfl_default_pos) %>%
    #Put in alphabetical order to match MFL desktop view
    arrange(mfl_player_name)
  
  return(list(players_df = players, curr_pos_changes_df = curr_pos_changes))
}


ADL_players_changes <- get_league_players(ADL24)
ADL_players_2024 <- ADL_players_changes$players_df
ADL_curr_pos_changes <- ADL_players_changes$curr_pos_changes_df

FAFL_players_changes <- get_league_players(FAFL24)
FAFL_players_2024 <- FAFL_players_changes$players_df
FAFL_curr_pos_changes <- FAFL_players_changes$curr_pos_changes_df


#Build df listing all current ESPN Fantasy positions.
espn_players_2024 <- ffscrapr::espn_players() %>%
  rename(espn_id = player_id,
         espn_pos = pos,
         espn_player_name = player_name,
         espn_team = team) %>%
  mutate(espn_clean_player_name = nflreadr::clean_player_names(espn_player_name),
         clean_team_name = nflreadr::clean_team_abbrs(espn_team),
         espn_pos = case_when(
           espn_pos == "P" ~ "PN",
           espn_pos == "K" ~ "PK",
           TRUE ~ espn_pos  # Keep original value for all other positions
         ))

#Join ESPN player list to MFL's
ADL_players_2024_with_espn <- ADL_players_2024 %>%
  full_join(., y = espn_players_2024 %>%
              select(c("espn_id",
                       "espn_player_name",
                       "espn_team",
                       "espn_pos")),
            by = c("espn_id")) %>%
  relocate(espn_pos, .after = mfl_default_pos)

ADL_espn_disagreements <- ADL_players_2024_with_espn %>%
  filter(league_pos != espn_pos)

view(ADL_espn_disagreements)

ADL_FAFL_disagreements <- ADL_players_2024 %>%
  rename(ADL_pos = league_pos) %>%
  left_join(., y = FAFL_players_2024 %>%
              rename(FAFL_pos = league_pos) %>%
              select(mfl_id,
                     FAFL_pos),
            by = "mfl_id") %>%
  filter(ADL_pos != FAFL_pos) %>%
  relocate(FAFL_pos, .after = ADL_pos)

view(ADL_FAFL_disagreements)
