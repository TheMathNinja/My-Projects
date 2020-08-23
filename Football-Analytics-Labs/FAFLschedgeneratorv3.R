library(tidyverse)
library(dplyr)
library(na.tools)
library(ggrepel)
library(ggimage)
library(caret)
library(lubridate)
library(ffscrapr)
library("data.table")

FAFL14 <- mfl_connect(season = 2014, league_id = 37677, rate_limit_number = 3, rate_limit_seconds = 6)
FAFL15 <- mfl_connect(season = 2015, league_id = 27312, rate_limit_number = 3, rate_limit_seconds = 6)
FAFL16 <- mfl_connect(season = 2016, league_id = 22686, rate_limit_number = 3, rate_limit_seconds = 6)
FAFL17 <- mfl_connect(season = 2017, league_id = 22686, rate_limit_number = 3, rate_limit_seconds = 6)
FAFL18 <- mfl_connect(season = 2018, league_id = 22686, rate_limit_number = 3, rate_limit_seconds = 6)
FAFL19 <- mfl_connect(season = 2019, league_id = 22686, rate_limit_number = 3, rate_limit_seconds = 6)
FAFL20 <- mfl_connect(season = 2020, league_id = 22686, rate_limit_number = 3, rate_limit_seconds = 6)

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

#Use For Loop to repeat this process for all other years
for(i in 14:20){
  
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
      mutate(pf_ratio_mean = points_for / mean(get(paste0("standings",i))$points_for))%>%
      mutate(pf_ratio_median = points_for / median(get(paste0("standings",i))$points_for))%>%
      mutate(pp_ratio_mean = potential_points / mean(get(paste0("standings",i))$potential_points))%>%
      mutate(pp_ratio_median = potential_points / median(get(paste0("standings",i))$potential_points))%>%
      mutate(allplay_sum = allplay_wins + allplay_losses + allplay_ties)
  )
  #Define RegSeasonData
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
           mutate(year = 2000+i)%>%
           select(franchise_id, owner_name, year, conference))

  #Add owner name to fullstandings data frame
  assign(
    paste0("GMstats",i),
    left_join(
      get(paste0("fullstandings",i)),
      get(paste0("GMnamekey",i)),
      by = "franchise_id")%>%
      select(owner_name, year, conference, franchise_name, allplay_wins, allplay_losses, allplay_ties, allplay_sum, allplay_winpct, points_for, pf_ratio_mean, pf_ratio_median, potential_points, pp_ratio_mean, pp_ratio_median, reg_adj_allplay_wins, reg_all_play_pct)
  )
  
  #This looks at whether or not an owner is a returning owner
  if(i != 14){
    assign(
      paste0("GMstats",i),
      get(paste0("GMstats",i)) %>% 
        mutate(
          returner = ifelse(owner_name %in% get(paste0("GMstats",i-1))$owner_name,1,0),
          returners = sum(returner)
        )
    )
  }
  
  #If it is 2014 (the first year available) we'll just set returner and returners to NA
  else{
    assign(
      paste0("GMstats",i),
      get(paste0("GMstats",i)) %>% 
        mutate(
          returner = NA,
          returners = NA
        )
    )
  }
  
}

#append "prev_" to all column names except owner
#define regAPW, returner, returners, returningAPW and append at end

#This basically looks at which owners are in the next year's league, and gets their APW from the next year
GMstats14 <- GMstats14 %>% 
  mutate(
    next_yr = ifelse(owner_name %in% GMstats15$owner_name, 1, 0)
  ) %>% 
  left_join(
    GMstats15 %>% select(owner_name, reg_adj_allplay_wins),
    by = 'owner_name'
  )

#This just renames the last column
names(GMstats14)[length(names(GMstats14))]<-"next_regAPW"

#Because I am lazy and this works I am just going to repeat this multiple times, instead of figuring out how to loop over this

GMstats15 <- GMstats15 %>% 
  mutate(
    next_yr = ifelse(owner_name %in% GMstats16$owner_name, 1, 0)
  ) %>% 
  left_join(
    GMstats16 %>% select(owner_name, reg_adj_allplay_wins),
    by = 'owner_name'
  )
names(GMstats15)[length(names(GMstats15))]<-"next_regAPW"


GMstats16 <- GMstats16 %>% 
  mutate(
    next_yr = ifelse(owner_name %in% GMstats17$owner_name, 1, 0)
  ) %>% 
  left_join(
    GMstats17 %>% select(owner_name, reg_adj_allplay_wins),
    by = 'owner_name'
  )
names(GMstats16)[length(names(GMstats16))]<-"next_regAPW"

GMstats17 <- GMstats17 %>% 
  mutate(
    next_yr = ifelse(owner_name %in% GMstats18$owner_name, 1, 0)
  ) %>% 
  left_join(
    GMstats18 %>% select(owner_name, reg_adj_allplay_wins),
    by = 'owner_name'
  )
names(GMstats17)[length(names(GMstats17))]<-"next_regAPW"

GMstats18 <- GMstats18 %>% 
  mutate(
    next_yr = ifelse(owner_name %in% GMstats19$owner_name, 1, 0)
  ) %>% 
  left_join(
    GMstats19 %>% select(owner_name, reg_adj_allplay_wins),
    by = 'owner_name'
  )
names(GMstats18)[length(names(GMstats18))]<-"next_regAPW"

franchises20 <- ff_franchises(FAFL20)
GMstats19 <- GMstats19 %>% 
  mutate(
    next_yr = ifelse(owner_name %in% franchises20$owner_name, 1, 0)
  )%>%
  mutate(filler_nextregAPW = NA)
names(GMstats19)[length(names(GMstats19))]<-"next_regAPW"

GMall <- bind_rows(GMstats14, GMstats15, GMstats16, GMstats17, GMstats18, GMstats19)
view(GMall)

write.csv(GMall, "C:/Users/filim/Documents/R/LeagueFeatures/FAFLschedgeneratorv3.csv", row.names = FALSE)