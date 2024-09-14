#Returns a full map of all current MFL & ESPN fantasy system players including NA values where one system omits the player
#Note: espn_players() does not return head coach ID's. Could be worth playing with get_endpoint to see if we can fetch them.
library(ffscrapr)
library(tidyverse)

load_mfl_espn_id_map <- function() {
  
  # First populate full MFL player list
  mfl_names_list <- ffscrapr::mfl_players() %>%
    select(player_id,
           player_name,
           pos,
           team,
           espn_id) %>%
    rename(mfl_id = player_id,
           mfl_name = player_name,
           mfl_team = team,
           mfl_pos = pos) %>%
    mutate(player_name_clean = nflreadr::clean_player_names(mfl_name),
           team_clean = nflreadr::clean_team_abbrs(mfl_team),
           pos_clean = mfl_pos,
           mfl_id = as.integer(mfl_id),
           espn_id = as.integer(espn_id)) %>%
    filter(mfl_id > 999)
  
  # Clean ESPN player list for join
  espn_names_list <- ffscrapr::espn_players() %>%
    rename(espn_id = player_id,
           espn_pos = pos,
           espn_name = player_name,
           espn_team = team) %>%
    mutate(player_name_clean = nflreadr::clean_player_names(espn_name),
           team_clean = nflreadr::clean_team_abbrs(espn_team),
           pos_clean = case_when(
             espn_pos == "K" ~ "PK",
             espn_pos == "P" ~ "PN",
             TRUE ~ espn_pos  # Keep the original name otherwise
           ))
  
  # Test 0.1: Check for any mis-matched name issues in known ID matches
  test_0_1_name_discrepancies <- mfl_names_list %>%
    inner_join(espn_names_list, by = "espn_id") %>%
    filter(player_name_clean.x != player_name_clean.y)
    view(test_0_1_name_discrepancies)
    
    # Print the number of NA cases at start
    na_cases_start <- mfl_names_list %>%
      filter(is.na(espn_id)) %>%
      nrow()
    cat("NA Cases at Start: ", na_cases_start, "\n")

  #Join 1: Incorporate Test 1 findings & join by player, team, position
  mfl_espn_join_1 <- mfl_names_list %>%
    #Fix Name Discrepancies found in Test 1
    mutate(mfl_name = case_when(
      mfl_id == 16321 ~ "DiCaprio Bootle",
      mfl_id == 16321 ~ "Chris Smith",
      mfl_id == 16374 ~ "Grant DuBose",
      TRUE ~ mfl_name  # Keep the original name if no match
    )) %>%
    left_join(espn_names_list %>%
                select(player_name_clean, team_clean, pos_clean, espn_id),
              by = c("player_name_clean", "team_clean", "pos_clean"))
  # Test 1.1: Check for duplicates of player/team/position (if we get some here, we will need to filter for is.na(espn_id) as we do later to avoid many-to-many join)
  test1_1_join_1_duplicates <- mfl_espn_join_1 %>%
    group_by(player_name_clean, team_clean, pos_clean) %>%
    filter(n() > 1) %>%
    arrange(player_name_clean, team_clean, pos_clean)
  view(test1_1_join_1_duplicates)
  # Test1.2: Check for ESPN ID contradictions in the two df's
  test1_2_join_1_id_contradictions <- mfl_espn_join_1 %>%
    filter(!is.na(espn_id.x) & !is.na(espn_id.y) & espn_id.x != espn_id.y)
  view(test1_2_join_1_id_contradictions)
  # View Join 2 successes & Print the number of NA cases resolved in the second join
  join_1_successes <- mfl_espn_join_1 %>%
    filter(is.na(espn_id.x) & !is.na(espn_id.y))
  view(join_1_successes)
  na_cases_resolved_join_1 <- join_1_successes %>%
    nrow()
  cat("Join 1 NA cases resolved: ", na_cases_resolved_join_1, "\n")
  # Resolve test issues and overwrite NA espn_id's
  mfl_espn_join_1 <- mfl_espn_join_1 %>%
    mutate(espn_id.x = case_when(
      mfl_id == 13691 ~ 3045220,
      TRUE ~ espn_id.x  # Keep the original name if no match
    )) %>%
    mutate(espn_id = coalesce(espn_id.x, espn_id.y)) %>%
    select(-espn_id.x, -espn_id.y)
  
  #Join 2: Join by name and position only
  # Filter rows where espn_id is NA before performing the join
  na_rows_pre_join_2 <- mfl_espn_join_1 %>%
    filter(is.na(espn_id))
  
  na_rows_joined_2 <- na_rows_pre_join_2 %>%
    left_join(espn_names_list %>%
                select(player_name_clean, pos_clean, espn_id, espn_team),
              by = c("player_name_clean", "pos_clean"))
  
  # Test 2.1: Check for duplicates of player/position after the second join
  test2_1_join_2_duplicates <- na_rows_joined_2 %>%
    group_by(player_name_clean, pos_clean) %>%
    filter(n() > 1) %>%
    arrange(player_name_clean, pos_clean)
  view(test2_1_join_2_duplicates)
  
  # View Join 2 successes & Print the number of NA cases resolved in the second join
  join_2_successes <- na_rows_joined_2 %>%
    filter(!is.na(espn_id.y))
  view(join_2_successes)
  na_cases_resolved_join_2 <- join_2_successes %>%
    nrow()
  cat("Join 2 NA cases resolved: ", na_cases_resolved_join_2, "\n")
  
  # Resolve test issues, overwrite NA espn_id's, and drop reference (espn_team) column
  na_rows_joined_2 <- na_rows_joined_2 %>%
    mutate(espn_id = coalesce(espn_id.x, espn_id.y)) %>%
    select(-espn_id.x, -espn_id.y, -espn_team)  # Drop unnecessary columns
  
  # Bind the updated NA rows back to the original dataframe
  mfl_espn_join_2 <- mfl_espn_join_1 %>%
    filter(!is.na(espn_id)) %>%  # Keep rows where espn_id.x is already filled
    bind_rows(na_rows_joined_2)    # Add the resolved NA rows
  
  #Join 3: Join by name and team only
  # Filter rows where espn_id is NA before performing the join
  na_rows_pre_join_3 <- mfl_espn_join_2 %>%
    filter(is.na(espn_id))
  
  na_rows_joined_3 <- na_rows_pre_join_3 %>%
    left_join(., y = espn_names_list %>%
                select(player_name_clean, team_clean, espn_pos, espn_id),
              by = c("player_name_clean", "team_clean"))
  
  # Test 3.1: Check for duplicates of player/team after the second join
  test3_1_join_3_duplicates <- na_rows_joined_3 %>%
    group_by(player_name_clean, team_clean) %>%
    filter(n() > 1) %>%
    arrange(player_name_clean, team_clean)
  view(test3_1_join_3_duplicates)
  
  # View Join 3 successes & Print the number of NA cases resolved in the join
  join_3_successes <- na_rows_joined_3 %>%
    filter(!is.na(espn_id.y))
  view(join_3_successes)
  na_cases_resolved_join_3 <- join_3_successes %>%
    nrow()
  cat("Join 3 NA cases resolved: ", na_cases_resolved_join_3, "\n")
  
  # Resolve test issues, overwrite NA espn_id's, and drop reference (espn_team) column
  na_rows_joined_3 <- na_rows_joined_3 %>%
    mutate(espn_id = coalesce(espn_id.x, espn_id.y)) %>%
    select(-espn_id.x, -espn_id.y, -espn_pos)  # Drop unnecessary columns
  
  # Bind the updated NA rows back to the original dataframe
  mfl_espn_join_3 <- mfl_espn_join_2 %>%
    filter(!is.na(espn_id)) %>%  # Keep rows where espn_id.x is already filled
    bind_rows(na_rows_joined_3)    # Add the resolved NA rows
  
  #Join 4: Join by name only
  # Filter rows where espn_id is NA before performing the join
  na_rows_pre_join_4 <- mfl_espn_join_3 %>%
    filter(is.na(espn_id))
  
  na_rows_joined_4 <- na_rows_pre_join_4 %>%
    left_join(., y = espn_names_list %>%
                select(player_name_clean, espn_team, espn_pos, espn_id),
              by = "player_name_clean")
  
  # Test 4.1: Check for duplicates of player_name after join
  test4_1_join_4_duplicates <- na_rows_joined_4 %>%
    group_by(player_name_clean) %>%
    filter(n() > 1) %>%
    arrange(player_name_clean)
  view(test4_1_join_4_duplicates)
  
  # View Join 4 successes & Print the number of NA cases resolved in the join
  join_4_successes <- na_rows_joined_4 %>%
    filter(!is.na(espn_id.y))
  view(join_4_successes)
  na_cases_resolved_join_4 <- join_4_successes %>%
    nrow()
  cat("Join 4 NA cases resolved: ", na_cases_resolved_join_4, "\n")
  
  # Resolve test issues, overwrite NA espn_id's, and drop reference (espn_team) column
  na_rows_joined_4 <- na_rows_joined_4 %>%
    mutate(espn_id = coalesce(espn_id.x, espn_id.y)) %>%
    select(-espn_id.x, -espn_id.y, -espn_team, -espn_pos)  # Drop unnecessary columns
  
  # Bind the updated NA rows back to the original dataframe
  mfl_espn_join_4 <- mfl_espn_join_3 %>%
    filter(!is.na(espn_id)) %>%                # Keep rows where espn_id.x is already filled
    bind_rows(na_rows_joined_4) %>%            # Add the resolved NA rows
    mutate(espn_id = as.integer(espn_id)) %>%  # Restore to int type from start
    arrange(mfl_id)                            # Restore row order
  
  return(mfl_espn_join_4)
}

# #Save df map to RDS file when there is an update; otherwise leave commented out
# saveRDS(load_mfl_espn_id_map(), file = "C:/Users/filim/Documents/R/FFAucAndDraft/mfl_espn_id_map.rds")
