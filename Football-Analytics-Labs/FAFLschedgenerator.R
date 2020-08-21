install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggimage")
install.packages("devtools")
install.packages("ffscrapr")
install.packages("data.table")

library(tidyverse)
library(tidyr)
library(dplyr)
library(na.tools)
library(ggrepel)
library(ggimage)
library(ggplot2)
library(caret)
library(lubridate)
library(ffscrapr)
library("data.table")

FAFL14 <- mfl_connect(season = 2014, league_id = 37677, user_name = "TheMathNinja", password = "Trapper7!", rate_limit_number = 3, rate_limit_seconds = 6)
FAFL15 <- mfl_connect(season = 2015, league_id = 27312, user_name = "TheMathNinja", password = "Trapper7!", rate_limit_number = 3, rate_limit_seconds = 6)
FAFL16 <- mfl_connect(season = 2016, league_id = 22686, user_name = "TheMathNinja", password = "Trapper7!", rate_limit_number = 3, rate_limit_seconds = 6)
FAFL17 <- mfl_connect(season = 2017, league_id = 22686, user_name = "TheMathNinja", password = "Trapper7!", rate_limit_number = 3, rate_limit_seconds = 6)
FAFL18 <- mfl_connect(season = 2018, league_id = 22686, user_name = "TheMathNinja", password = "Trapper7!", rate_limit_number = 3, rate_limit_seconds = 6)
FAFL19 <- mfl_connect(season = 2019, league_id = 22686, user_name = "TheMathNinja", password = "Trapper7!", rate_limit_number = 3, rate_limit_seconds = 6)

#define cumulative_standings function
cumulative_standings <- function(conn, week_select){
  
  df_schedule <- ff_schedule(conn)
  
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

#Parse out 2014 by itself
#Create Standings and Franchise Data Frames
standings14 <- ff_standings(FAFL14)
standings14 <- standings14 %>%
  mutate(pf_ratio = points_for / mean(standings14$points_for))%>%
  mutate(pp_ratio = potential_points / mean(standings14$potential_points))%>%
  mutate(allplay_sum = allplay_wins + allplay_losses + allplay_ties)
#JoinRegSeasonData
regseasonstandings14 <- cumulative_standings(FAFL14,12)
names(regseasonstandings14) <- paste0("reg_", names(regseasonstandings14))
names(regseasonstandings14)[names(regseasonstandings14)=="reg_franchise_id"] <- "franchise_id"
fullstandings14 <- left_join(standings14, regseasonstandings14, by = "franchise_id")

#Create franchisesXX variable
franchises14 <- ff_franchises(FAFL14)
#create a map from franchise ID to owner name
GMnamekey14 <- franchises14%>%
  select(franchise_id, owner_name, conference)
#Add owner name to standings data frame
GMstats14 <- left_join(fullstandings14, GMnamekey14, by = "franchise_id")%>%
  select(owner_name, conference, franchise_name, allplay_wins, allplay_losses, allplay_ties, allplay_sum, allplay_winpct, points_for, pf_ratio, potential_points, pp_ratio, reg_adj_allplay_wins, reg_all_play_pct)
#rename columns to add year suffix
names(GMstats14) <- paste0(names(GMstats14), "14")
names(GMstats14)[names(GMstats14)=="owner_name14"] <- "owner_name"
view(GMstats14)
 
#Use For Loop to repeat this process for all other years
for(i in 15:19){
  #Create standingsXX variable
  assign(
    paste0("standings",i),
         ff_standings(get(paste0("FAFL",i)))
    )
  ##Update standingsXX to include pf_ratio and pp_ratio
  assign(
    paste0("standings",i), 
    get(paste0("standings",i))
    %>%
    mutate(pf_ratio = points_for / mean(get(paste0("standings",i))$points_for))%>%
    mutate(pp_ratio = potential_points / mean(get(paste0("standings",i))$potential_points))%>%
    mutate(allplay_sum = allplay_wins + allplay_losses + allplay_ties)
  )
  #JoinRegSeasonData
  assign(
    paste0("regseasonstandings",i), 
    cumulative_standings(get(paste0("FAFL",i)),12)
  )
  #Rename Columns to Add "reg_" Prefix
  colnameswithpre <- paste0("reg_",names(get(paste0("regseasonstandings",i))))
  setnames(get(paste0("regseasonstandings",i)),colnameswithpre)
  #Remove Prefix from franchise_id variable
  setnames(get(paste0("regseasonstandings",i)),1,"franchise_id")
  #Create Full Standings by joining reg season data
  assign(paste0("fullstandings",i), left_join(get(paste0("standings",i)),get(paste0("regseasonstandings",i)), by = "franchise_id"))
  
  #Create franchisesXX variable
  assign(paste0("franchises",i),ff_franchises(get(paste0("FAFL",i))))
  #Map franchise ID to owner name
  assign(paste0("GMnamekey",i),get(paste0("franchises",i))%>%
           select(franchise_id, owner_name, conference))
  #Add owner name to fullstandings data frame, make returner variable
  assign(
    paste0("GMstats",i),
    left_join(
      get(paste0("fullstandings",i)),
      get(paste0("GMnamekey",i)), 
      by = "franchise_id")%>%
      mutate(returner = if_else(owner_name %in% get(paste0("GMstats",i-1))$owner_name, 1, 0))%>%
      mutate(prev_regAPW = with(get(paste0("GMstats",i-1)),get(paste0("reg_adj_allplay_wins",i-1))[match(owner_name, get(paste0("GMstats",i-1))$owner_name)]))%>%
      select(owner_name, returner, conference, franchise_name, prev_regAPW, allplay_wins, allplay_losses, allplay_ties, allplay_sum, allplay_winpct, points_for, pf_ratio, potential_points, pp_ratio, reg_adj_allplay_wins, reg_all_play_pct)
  )
  #rename columns to add year suffix
  colnameswithsuff <- paste0(names(get(paste0("GMstats",i))),i)
  setnames(get(paste0("GMstats",i)),colnameswithsuff)
  #Remove suffix from Owner name variable
  setnames(get(paste0("GMstats",i)),1,"owner_name")
  
  #Define returning stats and returner AP wins
  assign(
    paste0("GMsummary",i),
    get(paste0("GMstats",i))%>%
      group_by(get(paste0("conference",i)), get(paste0("returner",i)))%>%
      summarise(
        n = n(),
        APwins = sum(get(paste0("reg_adj_allplay_wins",i)))
      )
  )
}
#View at least one finished product to check
view(GMstats15)
view(GMsummary15)
#Join data sets
GM1415 <- full_join(GMstats14, GMstats15, by = "owner_name")
GM1416 <- full_join(GM1415, GMstats16, by = "owner_name")
GM1417 <- full_join(GM1416, GMstats17, by = "owner_name")
GM1418 <- full_join(GM1417, GMstats18, by = "owner_name")
GMall <- full_join(GM1418, GMstats19, by = "owner_name")

write.csv(GMall, "C:/Users/filim/Documents/R/LeagueFeatures/FAFLschedgenerator.csv", row.names = FALSE)

#Now we seek to predict reg_all_play_yearN from variables in year N-1
GMstats14$reg_adj_allplay_wins14[match(GMstats15$owner_name,GMstats14$owner_name)]
GMstats14[["reg_adj_allplay_wins14"]][match(GMstats15$owner_name,GMstats14$owner_name)]
with(GMstats14,reg_adj_allplay_wins14[match(GMstats15$owner_name,GMstats14$owner_name)])
