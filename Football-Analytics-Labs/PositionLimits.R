library(tidyverse)

#work with PFF snap counts
pff_snapcounts_raw_2019 <- readRDS(url("https://github.com/christianlohr9/FAFL/blob/main/data/pff_main_data2019.rds?raw=true"))
pff_snapcounts_raw_2020 <- readRDS("C:/Users/filim/Documents/R/LeagueFeatures/PositionLimits/pff_main_data_fafl2020.rds")

write.csv(pff_snapcounts_raw_2020, "C:/Users/filim/Documents/R/LeagueFeatures/PositionLimits/pff_snapcounts_raw_2020.csv", row.names = FALSE)

pff_snapcounts_pretty_2020 <- pff_snapcounts_raw_2020 %>%
  select(nflfastR.pff_id,
         nflfastR.gsis_id,
         dob,
         jersey_number,
         player_franchise_name,
         first_name,
         last_name,
         #draft_season,
         nflfastR.position,
         nflfastR.depth_chart_position,
         position_player,
         position_game,
         season,
         week,
         game_id,
         home_franchise_name,
         away_franchise_name,
         ball_side,
         offense,
         defense,
         special_teams,
         run,
         pass,
         pass_route,
         run_block,
         pass_block,
         run_defense,
         pass_rush,
         coverage,
         field_goal,
         field_goal_blocking,
         field_goal_kicking,
         kickoff_coverage,
         kickoff_kicking,
         kickoff_return_blocking,
         kickoff_returning,
         punt_coverage,
         punt_punting,
         punt_return_blocking,
         punt_returning) %>%
  mutate(OffPlayer = ifelse(position_player == "QB" |
                              position_player == "HB" |
                              position_player == "FB" |
                              position_player == "WR" |
                              position_player == "TE" |
                              position_player == "T" |
                              position_player == "G" |
                              position_player == "C",
                            1, 0)) %>%
  mutate(DefPlayer = ifelse(position_player == "DI" |
                              position_player == "ED" |
                              position_player == "LB" |
                              position_player == "CB" |
                              position_player == "S",
                            1, 0)) %>%
  mutate(STPlayer = ifelse(position_player == "K" |
                              position_player == "P" |
                              position_player == "LS",
                            1, 0)) %>%
  mutate(poscheck = OffPlayer + DefPlayer + STPlayer) %>%
  mutate(snaps = case_when(OffPlayer == 1 ~ offense,
                           DefPlayer == 1 ~ defense,
                           STPlayer == 1 ~ special_teams)
         ) %>%
  #Classify which on-field unit (home O vs. away D or away O vs. home D) player belongs to
  mutate(unit = case_when(
                          (player_franchise_name == home_franchise_name & OffPlayer == 1 |
                          player_franchise_name == away_franchise_name & DefPlayer == 1) ~ "home_off",
                          (player_franchise_name == home_franchise_name & DefPlayer == 1 | 
                          player_franchise_name == away_franchise_name & OffPlayer == 1) ~ "home_def",
                          STPlayer == 1 ~ "special"
                          )
         )

#check that poscheck == 1 for all and that unit is always 1 of 3 values for poscheck == 1
#Group errors at the end
pff_snapcounts_pretty %>% group_by(poscheck, unit) %>%
  summarise(n = n()) %>%
  ungroup()

write.csv(pff_snapcounts_pretty, "C:/Users/filim/Documents/R/LeagueFeatures/PositionLimits/pff_snapcounts_pretty.csv", row.names = FALSE)

#OffHomeSnaps and #DefHomeSnaps variables
#top_n function will help here for top 11/12/13 players by snap count
#Use various dplyr rank functions to get what we need https://dplyr.tidyverse.org/reference/ranking.html

pff_fantasy_snapcounts <- pff_snapcounts_pretty %>%
  group_by(game_id, unit) %>%
  mutate(unit_max_snaps = max(snaps)) %>%
  ungroup() %>%
  group_by(season, player_franchise_name, week, OffPlayer) %>%
  mutate(side_max_snaps = max(snaps)) %>%
  mutate(max_snaps = max(unit_max_snaps, side_max_snaps)) %>%
  mutate(snap_pct = snaps/max_snaps) %>%
  mutate(mfl_position = if_else(position_player == "FB" | position_player == "HB", "RB", position_player)) %>%
  mutate(idp_group = case_when((position_player == "DI" | position_player == "ED") ~ "DL",
                               position_player == "LB" ~ "LB",
                               (position_player == "CB" | position_player == "S") ~ "DB")
         ) %>%
  filter(position_player != "C",
         position_player != "G",
         position_player != "T",
         STPlayer == 0) %>%
  mutate(snap_order = rank(desc(snaps), ties.method = "first")) %>%
  mutate(snap_rank = rank(desc(snaps), ties.method = "min")) %>%
  ungroup() %>%
  group_by(season, player_franchise_id, week, OffPlayer, snap_rank) %>%
  mutate(max_order_in_rank = max(snap_order)) %>%
  ungroup() %>%
  group_by(season, player_franchise_id, week, OffPlayer) %>%
  mutate(top_11_D = case_when(
    (DefPlayer == 1 & snap_rank < 11 & max_order_in_rank < 11) ~ 1,
    (DefPlayer == 1 & snap_rank <= 11 & max_order_in_rank >= 11) ~ (11 - snap_rank + 1)/(max_order_in_rank - snap_rank + 1),
    (DefPlayer == 0 | (DefPlayer == 1 & snap_rank > 11)) ~ 0
  )) %>%
  mutate(top_12_D = case_when(
    (DefPlayer == 1 & snap_rank < 12 & max_order_in_rank < 12) ~ 1,
    (DefPlayer == 1 & snap_rank <= 12 & max_order_in_rank >= 12) ~ (12 - snap_rank + 1)/(max_order_in_rank - snap_rank + 1),
    (DefPlayer == 0 | (DefPlayer == 1 & snap_rank > 12)) ~ 0
  )) %>%
  mutate(top_13_D = case_when(
    (DefPlayer == 1 & snap_rank < 13 & max_order_in_rank < 13) ~ 1,
    (DefPlayer == 1 & snap_rank <= 13 & max_order_in_rank >= 13) ~ (13 - snap_rank + 1)/(max_order_in_rank - snap_rank + 1),
    (DefPlayer == 0 | (DefPlayer == 1 & snap_rank > 13)) ~ 0
  )) %>%
  mutate(top_6_O = case_when(
    (OffPlayer == 1 & snap_rank < 6 & max_order_in_rank < 6) ~ 1,
    (OffPlayer == 1 & snap_rank <= 6 & max_order_in_rank >= 6) ~ (6 - snap_rank + 1)/(max_order_in_rank - snap_rank + 1),
    (OffPlayer == 0 | (OffPlayer == 1 & snap_rank > 6)) ~ 0
  )) %>%
  mutate(top_7_O = case_when(
    (OffPlayer == 1 & snap_rank < 7 & max_order_in_rank < 7) ~ 1,
    (OffPlayer == 1 & snap_rank <= 7 & max_order_in_rank >= 7) ~ (7 - snap_rank + 1)/(max_order_in_rank - snap_rank + 1),
    (OffPlayer == 0 | (OffPlayer == 1 & snap_rank > 7)) ~ 0
  )) %>%
  mutate(top_8_O = case_when(
    (OffPlayer == 1 & snap_rank < 8 & max_order_in_rank < 8) ~ 1,
    (OffPlayer == 1 & snap_rank <= 8 & max_order_in_rank >= 8) ~ (8 - snap_rank + 1)/(max_order_in_rank - snap_rank + 1),
    (OffPlayer == 0 | (OffPlayer == 1 & snap_rank > 8)) ~ 0
  )) %>%
  arrange(desc(snaps), .by_group = T) %>%
  select(nflfastR.pff_id,
         nflfastR.gsis_id,
         dob,
         jersey_number,
         player_franchise_name,
         first_name,
         last_name,
         #draft_season,
         nflfastR.position,
         nflfastR.depth_chart_position,
         position_player,
         position_game,
         season,
         week,
         game_id,
         home_franchise_name,
         away_franchise_name,
         ball_side,
         snaps,
         side_max_snaps,
         unit_max_snaps,
         max_snaps,
         snap_order,
         snap_rank,
         max_order_in_rank,
         snap_pct,
         top_11_D,
         top_12_D,
         top_13_D,
         top_6_O,
         top_7_O,
         top_8_O,
         OffPlayer,
         DefPlayer,
         STPlayer,
         poscheck,
         offense,
         defense,
         special_teams,
         run,
         pass,
         pass_route,
         run_block,
         pass_block,
         run_defense,
         pass_rush,
         coverage,
         field_goal,
         field_goal_blocking,
         field_goal_kicking,
         kickoff_coverage,
         kickoff_kicking,
         kickoff_return_blocking,
         kickoff_returning,
         punt_coverage,
         punt_punting,
         punt_return_blocking,
         punt_returning) %>%
  ungroup()

#Check code for games where max O and max D are different within unit
side_discrep <- pff_fantasy_snapcounts %>%
  filter(side_max_snaps != unit_max_snaps)
view(side_discrep)


write.csv(pff_fantasy_snapcounts, "C:/Users/filim/Documents/R/LeagueFeatures/PositionLimits/pff_fantasy_snapcounts.csv", row.names = FALSE)

#Show snap_pct by snap rank to compare total starters on off. and def.
snap_rank_breakdown <- pff_fantasy_snapcounts %>%
  group_by(season,
           OffPlayer,
           snap_order) %>%
  summarise(mean_snap_pct = mean(snap_pct),
            n = n())

view(snap_rank_breakdown)

#Show positional usage broken out by week and team
#Count the number of Top 11/12/13 per position per game
pos_usage_summary <- pff_fantasy_snapcounts %>% 
  group_by(season,
           week,
           player_franchise_id,
           OffPlayer,
           mfl_position) %>%
  summarise(pos_top11D = sum(top_11_D),
            pos_top12D = sum(top_12_D),
            pos_top13D = sum(top_13_D),
            pos_top6O = sum(top_6_O),
            pos_top7O = sum(top_7_O),
            pos_top8O = sum(top_8_O)) %>%
  ungroup()
view(pos_usage_summary)

#Show team-by-team results in given season of positional usage
#Should find a better way to calculate partial season (i.e. sub out "15" for weeks played by that team in data set)
#Use janitor tabyl function to create DL, Front7, and DB sum rows?
team_usage_summary <- pos_usage_summary %>%
  group_by(season,
           player_franchise_id,
           OffPlayer,
           mfl_position) %>%
  summarise(team_top_11D_per_game = sum(pos_top11D)/15,
            team_top_12D_per_game = sum(pos_top12D)/15,
            team_top_13D_per_game = sum(pos_top13D)/15,
            team_top_6O_per_game = sum(pos_top6O)/15,
            team_top_7O_per_game = sum(pos_top7O)/15,
            team_top_8O_per_game = sum(pos_top8O)/15) %>%
  ungroup()
view(team_usage_summary)

league_usage_summary <- team_usage_summary %>%
  group_by(season,
           OffPlayer,
           mfl_position) %>%
  summarise(league_top_11D_per_game = sum(team_top_11D_per_game)/32,
            league_top_12D_per_game = sum(team_top_12D_per_game)/32,
            league_top_13D_per_game = sum(team_top_13D_per_game)/32,
            league_top_6O_per_game = sum(team_top_6O_per_game)/32,
            league_top_7O_per_game = sum(team_top_7O_per_game)/32,
            league_top_8O_per_game = sum(team_top_8O_per_game)/32,)
view(league_usage_summary)

#View Whole-League Patterns by Displaying Various Percentile of Position Usage Frequency
league_usage_summary_ranks <- team_usage_summary %>%
  group_by(season,
           OffPlayer,
           mfl_position) %>%
  summarise(No32D = sort(team_top_12D_per_game, decreasing = TRUE)[32],
            No31D = sort(team_top_12D_per_game, decreasing = TRUE)[31],
            No29D = sort(team_top_12D_per_game, decreasing = TRUE)[29],
            No27D = sort(team_top_12D_per_game, decreasing = TRUE)[27],
            No17D = sort(team_top_12D_per_game, decreasing = TRUE)[17],
            No16D = sort(team_top_12D_per_game, decreasing = TRUE)[16],
            No6D = sort(team_top_12D_per_game, decreasing = TRUE)[6],
            No4D = sort(team_top_12D_per_game, decreasing = TRUE)[4],
            No2D = sort(team_top_12D_per_game, decreasing = TRUE)[2],
            No1D = sort(team_top_12D_per_game, decreasing = TRUE)[1],
            No32O = sort(team_top_7O_per_game, decreasing = TRUE)[32],
            No31O = sort(team_top_7O_per_game, decreasing = TRUE)[31],
            No29O = sort(team_top_7O_per_game, decreasing = TRUE)[29],
            No27O = sort(team_top_7O_per_game, decreasing = TRUE)[27],
            No17O = sort(team_top_7O_per_game, decreasing = TRUE)[17],
            No16O = sort(team_top_7O_per_game, decreasing = TRUE)[16],
            No6O = sort(team_top_7O_per_game, decreasing = TRUE)[6],
            No4O = sort(team_top_7O_per_game, decreasing = TRUE)[4],
            No2O = sort(team_top_7O_per_game, decreasing = TRUE)[2],
            No1O = sort(team_top_7O_per_game, decreasing = TRUE)[1])
            
view(league_usage_summary_ranks)

#Repeat the same process, but grouping DL and DB
#View Whole-League Patterns by Displaying Various Percentile of Position Usage Frequency
league_usage_summary_ranks_grouped <- team_usage_summary %>%
  group_by(season,
           OffPlayer,
           idp_group) %>%
  summarise(No32D = sort(team_top_12D_per_game, decreasing = TRUE)[32],
            No31D = sort(team_top_12D_per_game, decreasing = TRUE)[31],
            No29D = sort(team_top_12D_per_game, decreasing = TRUE)[29],
            No27D = sort(team_top_12D_per_game, decreasing = TRUE)[27],
            No17D = sort(team_top_12D_per_game, decreasing = TRUE)[17],
            No16D = sort(team_top_12D_per_game, decreasing = TRUE)[16],
            No6D = sort(team_top_12D_per_game, decreasing = TRUE)[6],
            No4D = sort(team_top_12D_per_game, decreasing = TRUE)[4],
            No2D = sort(team_top_12D_per_game, decreasing = TRUE)[2],
            No1D = sort(team_top_12D_per_game, decreasing = TRUE)[1],
            No32O = sort(team_top_7O_per_game, decreasing = TRUE)[32],
            No31O = sort(team_top_7O_per_game, decreasing = TRUE)[31],
            No29O = sort(team_top_7O_per_game, decreasing = TRUE)[29],
            No27O = sort(team_top_7O_per_game, decreasing = TRUE)[27],
            No17O = sort(team_top_7O_per_game, decreasing = TRUE)[17],
            No16O = sort(team_top_7O_per_game, decreasing = TRUE)[16],
            No6O = sort(team_top_7O_per_game, decreasing = TRUE)[6],
            No4O = sort(team_top_7O_per_game, decreasing = TRUE)[4],
            No2O = sort(team_top_7O_per_game, decreasing = TRUE)[2],
            No1O = sort(team_top_7O_per_game, decreasing = TRUE)[1])

view(league_usage_summary_ranks)
