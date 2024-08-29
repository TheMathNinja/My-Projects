remotes::install_github("ffverse/ffscrapr", ref = "dev")

library(tidyverse)
library(lubridate)
library(ffscrapr)
ffscrapr::ffverse_sitrep()

options(scipen = 9999)

#Define cumulative_standings function
cumulative_standings <- function(conn, week_select){
  
  df_schedule <- ff_schedule(conn)
  
  ##Errorhandling for schedule without a franchise score
  if(is.null(df_schedule[["franchise_score"]])){return(tibble(franchise_id = NA,
                                                              points_for = NA,
                                                              adj_allplay_wins = NA,
                                                              all_play_games = NA,
                                                              all_play_pct = NA))}
  
  df_schedule %>%
    filter(week <= week_select) %>% 
    filter(!is.na(franchise_id))%>%
    arrange(week,franchise_score) %>% 
    group_by(week) %>% 
    mutate(adj_allplay_wins = rank(franchise_score)-1,
           all_play_games = n()-1) %>% 
    ungroup() %>% 
    group_by(franchise_id) %>% 
    summarise(
      points_for = sum(franchise_score,na.rm = TRUE),
      adj_allplay_wins = sum(adj_allplay_wins),
      all_play_games = sum(all_play_games),
      all_play_pct = adj_allplay_wins/all_play_games
    ) 
}

# Create list of conn objects for looping (16-game seasons)
fafl_connections_16g <- list(
  FAFL14 <- mfl_connect(season = 2014, league_id = 37677, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL15 <- mfl_connect(season = 2015, league_id = 27312, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL16 <- mfl_connect(season = 2016, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL17 <- mfl_connect(season = 2017, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL18 <- mfl_connect(season = 2018, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL19 <- mfl_connect(season = 2019, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL20 <- mfl_connect(season = 2020, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6)
)

# Create list of conn objects for looping (17-game seasons)
fafl_connections_17g <- list(
  FAFL21 <- mfl_connect(season = 2021, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL22 <- mfl_connect(season = 2022, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL23 <- mfl_connect(season = 2023, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6),
  FAFL24 <- mfl_connect(season = 2024, league_id = 22686, user_name = "TheMathNinja", password = "Trapper17!", rate_limit_number = 3, rate_limit_seconds = 6)
)

# Create holding tables for all_standings data
all_standings_16g <- tibble()
all_standings_17g <- tibble()

#Use For Loop to build 16-game seasons
#We want ffscrapr to update ff_standings() to include Off, Def, Potential Pts etc. for more data
#Therefore Will keep ff_standings() in code for now even though it serves no function
for(i in fafl_connections_16g){
  
  standings <- ffscrapr::ff_standings(i)
  regseasonstandings <- cumulative_standings(i, 12) %>%
    rename_with(~ str_c("reg_", .), all_of("points_for"):last_col())
  fullseasonstandings <- cumulative_standings(i, 16) %>%
    rename_with(~ str_c("full_", .), all_of("points_for"):last_col())
  franchises <- ffscrapr::ff_franchises(i)
  
  complete_standings <- standings %>% 
    left_join(regseasonstandings, by = "franchise_id") %>% 
    left_join(fullseasonstandings, by = "franchise_id") %>% 
    left_join(franchises, by = c("franchise_id", "franchise_name")) %>% 
    mutate(season = i$season)
  
  all_standings_16g <- bind_rows(all_standings_16g, complete_standings)
  
}

#Use For Loop to build 17-game seasons
#We want ffscrapr to update ff_standings() to include Off, Def, Potential Pts etc. for more data
#Therefore Will keep ff_standings() in code for now even though it serves no function
for(i in fafl_connections_17g){
  
  standings <- ffscrapr::ff_standings(i)
  regseasonstandings <- cumulative_standings(i, 12) %>%
    rename_with(~ str_c("reg_", .), all_of("points_for"):last_col())
  fullseasonstandings <- cumulative_standings(i, 17) %>%
    rename_with(~ str_c("full_", .), all_of("points_for"):last_col())
  franchises <- ffscrapr::ff_franchises(i)
  
  complete_standings <- standings %>% 
    left_join(regseasonstandings, by = "franchise_id") %>% 
    left_join(fullseasonstandings, by = "franchise_id") %>% 
    left_join(franchises, by = c("franchise_id", "franchise_name")) %>% 
    mutate(season = i$season)
  
  all_standings_17g <- bind_rows(all_standings_17g, complete_standings)
  
}

all_standings_raw <- bind_rows(all_standings_16g,
                           all_standings_17g)
#Put in RDS form to take scraping load off MFL API
saveRDS(all_standings_raw, "C:/Users/filim/Documents/R/LeagueFeatures/Scheduler/all_standings_raw.rds")


# CODE BELOW IS TO COMPLETE ALL_STANDINGS
all_standings_raw <- readRDS("C:/Users/filim/Documents/R/LeagueFeatures/Scheduler/all_standings_raw.rds") %>%
  # Convert conference to numeric for later analysis
  mutate(conference = as.numeric(conference))

# Define the columns to be carried over
cols_to_carry <- c("conference", 
                   "reg_points_for", 
                   "reg_adj_allplay_wins",
                   "reg_all_play_games",
                   "reg_all_play_pct",
                   "full_points_for", 
                   "full_adj_allplay_wins",
                   "full_all_play_games",
                   "full_all_play_pct")

# Create the previous season dataframe
previous_season <- all_standings_raw %>%
  mutate(season = season + 1) %>%
  select(owner_name, email, username, season, all_of(cols_to_carry))

# Expand the usernames to handle comma-separated values
expand_usernames <- function(df) {
  df %>%
    separate_rows(username, sep = ",\\s*") %>%
    distinct()
}

expanded_all_standings <- expand_usernames(all_standings_raw)
expanded_previous_season <- expand_usernames(previous_season)

# Filter out rows with NA usernames before joining
expanded_previous_season_filtered <- expanded_previous_season %>% filter(!is.na(username))

# Perform the joins separately
join_by_name <- all_standings_raw %>%
  left_join(expanded_previous_season, by = c("owner_name", "season"), suffix = c("", "_prev"))

join_by_email <- all_standings_raw %>%
  left_join(expanded_previous_season, by = c("email", "season"), suffix = c("", "_prev"))

join_by_username <- expanded_all_standings %>%
  filter(!is.na(username)) %>%
  left_join(expanded_previous_season_filtered, by = c("username", "season"), suffix = c("", "_prev"))

# Combine the results, ensuring that we do not lose any matches
joined_data <- bind_rows(join_by_name, join_by_email, join_by_username) %>%
  distinct() %>%
  group_by(owner_name, email, username, season) %>%
  summarize(across(everything(), ~ first(na.omit(.)), .names = "grp_{.col}"), .groups = 'drop')

# Use the joined data to create the returner column, add previous season data, and calculate conference-related variables
all_standings <- all_standings_raw %>%
  left_join(joined_data, by = c("owner_name", "email", "username", "season")) %>%
  mutate(returner = if_else(
    season == 2014,
    NA_integer_,
    as.integer(!is.na(grp_reg_points_for_prev))
  )) %>%
  mutate(across(all_of(cols_to_carry), 
                ~ if_else(returner == 1, 
                          get(paste0("grp_", cur_column(), "_prev")), 
                          NA_real_), 
                .names = "prev_{.col}")) %>%
  mutate(rookie = if_else(is.na(returner), NA_integer_, 1L - returner)) %>%
  group_by(season, conference) %>%
  mutate(conf_rookies = sum(rookie, na.rm = FALSE),
         conf_returners = sum(returner, na.rm = FALSE),
         conf_ret_reg_all_play_pct = mean(prev_reg_all_play_pct, na.rm = TRUE),
         conf_ret_full_all_play_pct = mean(prev_full_all_play_pct, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(season) %>%
  mutate(nonconf_ret_reg_all_play_pct = map_dbl(conference, ~mean(conf_ret_reg_all_play_pct[conference != .x], na.rm = TRUE)),
         nonconf_ret_full_all_play_pct = map_dbl(conference, ~mean(conf_ret_full_all_play_pct[conference != .x], na.rm = TRUE)),
         nonconf_rookies = sum(rookie, na.rm = FALSE) - conf_rookies,
         nonconf_returners = sum(returner, na.rm = FALSE) - conf_returners) %>%
  ungroup() %>%
  select(-starts_with("grp_")) %>%
  relocate(owner_name, email, username, conference, season, returner, rookie, 
           conf_rookies, conf_returners, nonconf_rookies, nonconf_returners, 
           conf_ret_reg_all_play_pct, conf_ret_full_all_play_pct,
           nonconf_ret_reg_all_play_pct, nonconf_ret_full_all_play_pct)

write.csv(all_standings, "C:/Users/filim/Documents/R/LeagueFeatures/Scheduler/FAFLschedgeneratorv4.csv", row.names = FALSE)

#Tests for missing data; both should be blank
#view(all_standings %>% filter(returner == 0 &
#                                !is.na (prev_reg_points_for)))
#view(all_standings %>% filter(returner == 1 &
#                                is.na (prev_reg_points_for)))


# Use recent / relevant seasons for prediction model.
# 2017-2023 marks consistent GMs. 2021-23 marks 17-game era.

returner_standings_17_23 <- all_standings %>% filter(season >= 2017 &
                                                  season <= 2023 &
                                                    returner == 1)
rookie_standings_17_23 <- all_standings %>% filter(season >= 2017 &
                                                       season <= 2023 &
                                                       returner == 0)
returner_standings_21_23 <- all_standings %>% filter(season >= 2021 &
                                                  season <= 2023 &
                                                    returner == 1)
rookie_standings_21_23 <- all_standings %>% filter(season >= 2017 &
                                                     season <= 2023 &
                                                     returner == 0)

RS_returner_pred <-  lm(reg_all_play_pct ~
                        + prev_reg_all_play_pct
                        + I(prev_reg_all_play_pct^2)
                        + log(prev_reg_all_play_pct),
                        data = returner_standings_17_23)

RS_rookie_pred <-  lm(reg_all_play_pct ~
                        + conf_rookies
                        + conf_ret_reg_all_play_pct
                        + nonconf_ret_reg_all_play_pct
                        + conference,
                        data = rookie_standings_17_23)


## FULL SUSPECTS (trim from here):
# RS_returner_pred <-  lm(reg_all_play_pct ~
#                           + conf_returners
#                         + I(conf_returners^2)
#                         + nonconf_returners
#                         + I(nonconf_returners^2)
#                         + conf_ret_reg_all_play_pct
#                         + conf_ret_full_all_play_pct
#                         + nonconf_ret_reg_all_play_pct
#                         + nonconf_ret_full_all_play_pct
#                         + conference
#                         + prev_conference
#                         + prev_reg_all_play_pct
#                         + I(prev_reg_all_play_pct^2)
#                         + log(prev_reg_all_play_pct)
#                         + prev_full_all_play_pct
#                         + I(prev_full_all_play_pct^2)
#                         + log(prev_full_all_play_pct),
#                         data = returner_standings_17_23)

##  Consider: Once we get more Points data: calc Z scores for PF, OP, PP, etc.

# 
# returnAPW_model <- lm(returner_regAPW ~ returning_regAPW + returners + returning_regAPW*returners, data=APWpred_all)
# summary(returnAPW_model)
# returnAPW_model_perteam <- lm(returner_regAPW_per_team ~ returning_regAPW_per_team + returners + returning_regAPW_per_team*returners, data=APWpred_all)
# summary(returnAPW_model_perteam)
# 
# plot(APWpred_all$returning_regAPW_per_team, APWpred_all$returner_regAPW_per_team)