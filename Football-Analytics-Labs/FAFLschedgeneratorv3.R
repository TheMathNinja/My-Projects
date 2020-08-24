library(tidyverse)
library(dplyr)
library(na.tools)
library(ggrepel)
library(ggimage)
library(caret)
library(lubridate)
library(ffscrapr)
library("data.table")

options(scipen = 9999)

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
          returners = sum(returner),
          rookie = ifelse(owner_name %in% get(paste0("GMstats",i-1))$owner_name,0,1),
          rookies = sum(rookie),
          NFC_returner = ifelse(returner == 1 & conference == "00", 1, 0),
          NFC_returners = sum(NFC_returner),
          NFC_rookie = ifelse(returner == 0 & conference == "00", 1, 0),
          NFC_rookies = sum(NFC_rookie),
          AFC_returner = ifelse(returner == 1 & conference == "01", 1, 0),
          AFC_returners = sum(AFC_returner),
          AFC_rookie = ifelse(returner == 0 & conference == "01", 1, 0),
          AFC_rookies = sum(AFC_rookie),
          conf_returners = ifelse(conference == "00", NFC_returners, AFC_returners),
          conf_rookies = ifelse(conference == "00", NFC_rookies, AFC_rookies),
          nonconf_returners = ifelse(conference == "00", AFC_returners, NFC_returners),
          nonconf_rookies = ifelse(conference == "00", AFC_rookies, NFC_rookies),
               )
    )
  }
  
  #For 2014 (inaugural year), we set returner-related variables to NA
  else{
    assign(
      paste0("GMstats",i),
      get(paste0("GMstats",i)) %>% 
        mutate(
          returner = NA,
          returners = NA,
          rookie = NA,
          rookies = NA,
          NFC_returner = NA,
          NFC_returners = NA,
          NFC_rookie = NA,
          NFC_rookies = NA,
          AFC_returner = NA,
          AFC_returners = NA,
          AFC_rookie = NA,
          AFC_rookies = NA,
          conf_returners = NA,
          conf_rookies = NA,
          nonconf_returners = NA,
          nonconf_rookies = NA
        )
    )
  }
  
}

#Create predictor df by grabbing initial league data (owner names and returner status)
for(i in 15:20){

  assign(paste0("APWpred",i),
         get(paste0("GMstats",i)) %>%
           select(owner_name, 
                  year, 
                  conference, 
                  franchise_name, 
                  returner, 
                  rookie, 
                  returners, 
                  rookies, 
                  NFC_returner, 
                  NFC_rookie, 
                  AFC_returner, 
                  AFC_rookie, 
                  NFC_returners, 
                  NFC_rookies, 
                  AFC_returners, 
                  AFC_rookies, 
                  conf_returners,
                  conf_rookies,
                  nonconf_returners,
                  nonconf_rookies,
                  reg_adj_allplay_wins) %>%
           #Tack on detailed individual owner data from previous year for returning owners
           left_join(get(paste0("GMstats",i-1)) %>% 
                       select(-(returner:nonconf_rookies)) %>%
                       rename_all(function(x) paste0("prev_", x))%>%
                       rename(owner_name = prev_owner_name),
                     by = 'owner_name') %>%
           #Create aggregate variables for league conditions (how many wins returning in league)
           mutate(returning_regAPW = sum(prev_reg_adj_allplay_wins, na.rm = TRUE))%>%
           mutate(returning_regAPW_per_team = returning_regAPW / returners)%>%
           mutate(returner_regAPW = sum(reg_adj_allplay_wins * returner))%>%
           mutate(returner_regAPW_per_team = returner_regAPW / returners)%>%
           mutate(rookie_regAPW = sum(reg_adj_allplay_wins * rookie))%>%
           mutate(rookie_regAPW_per_team = rookie_regAPW / rookies)%>%
           #Make same variables at conference level
           mutate(returning_conf_regAPW = 
              ifelse(
                    conference == "00",
                    sum(prev_reg_adj_allplay_wins * NFC_returner, na.rm = TRUE),
                    sum(prev_reg_adj_allplay_wins * AFC_returner, na.rm = TRUE)
                    )
                 )%>%
           mutate(returning_conf_regAPW_per_team = returning_conf_regAPW / conf_returners)%>%
           mutate(returner_conf_regAPW = 
                    ifelse(
                      conference == "00",
                      sum(reg_adj_allplay_wins * NFC_returner),
                      sum(reg_adj_allplay_wins * AFC_returner)
                    )
                    )%>%
           mutate(returner_conf_regAPW_per_team = returner_conf_regAPW / conf_returners)%>%
           mutate(rookie_conf_regAPW = 
                    ifelse(
                      conference == "00",
                      sum(reg_adj_allplay_wins * NFC_rookie),
                      sum(reg_adj_allplay_wins * AFC_rookie)
                    )
           )%>%
           mutate(rookie_conf_regAPW_per_team = rookie_conf_regAPW / conf_rookies)%>%
         #Make same variables for non-conference
         mutate(returning_nonconf_regAPW = 
                  ifelse(
                    conference == "00",
                    sum(prev_reg_adj_allplay_wins * AFC_returner, na.rm = TRUE),
                    sum(prev_reg_adj_allplay_wins * NFC_returner, na.rm = TRUE)
                  )
         )%>%
           mutate(returning_nonconf_regAPW_per_team = returning_nonconf_regAPW / nonconf_returners)%>%
           mutate(returner_nonconf_regAPW = 
                    ifelse(
                      conference == "00",
                      sum(reg_adj_allplay_wins * AFC_returner),
                      sum(reg_adj_allplay_wins * NFC_returner)
                    )
           )%>%
           mutate(returner_nonconf_regAPW_per_team = returner_nonconf_regAPW / nonconf_returners)%>%
           mutate(rookie_nonconf_regAPW = 
                    ifelse(
                      conference == "00",
                      sum(reg_adj_allplay_wins * AFC_rookie),
                      sum(reg_adj_allplay_wins * NFC_rookie)
                    )
           )%>%
           mutate(rookie_nonconf_regAPW_per_team = rookie_nonconf_regAPW / nonconf_rookies)
         )
}

APWpred_all <- bind_rows(APWpred15, APWpred16, APWpred17, APWpred18, APWpred19)
view(APWpred_all)

write.csv(APWpred_all, "C:/Users/filim/Documents/R/LeagueFeatures/FAFLschedgeneratorv3.csv", row.names = FALSE)

rookiepred <- APWpred_all %>%
  filter(returner == 0)

vetpred <- APWpred_all %>%
  filter(returner == 1)

#Build rookie APW predictor; consider trying prev 16/17-week AllPlay, not just reg_allplay
rookie_APW_model <- lm(rookie_conf_regAPW ~ conf_rookies + nonconf_rookies + returning_conf_regAPW + returning_nonconf_regAPW, data=rookiepred)
summary(rookie_APW_model)
rookie_APW_model_perteam <- lm(rookie_conf_regAPW_per_team ~ returning_conf_regAPW_per_team*conf_rookies + returning_nonconf_regAPW_per_team*nonconf_rookies, data=rookiepred)
summary(rookie_APW_model_perteam)
#Below is our final rookie model
rookie_APW_model_perteam_simple <- lm(rookie_conf_regAPW_per_team ~ returning_conf_regAPW_per_team + returning_nonconf_regAPW_per_team + conf_rookies, data=rookiepred)
summary(rookie_APW_model_perteam_simple)

plot(APWpred_all$rookies, APWpred_all$rookie_regAPW_per_team)
plot(APWpred_all$conf_rookies, APWpred_all$rookie_conf_regAPW_per_team)

#Build Returner APW predictor
returner_APW_model_full <- lm(reg_adj_allplay_wins ~ 
                           prev_allplay_winpct + 
                           prev_pf_ratio_mean + 
                           prev_pf_ratio_median + 
                           prev_pp_ratio_mean + 
                           prev_pp_ratio_median + 
                           prev_reg_all_play_pct + 
                           conf_returners + 
                           returning_conf_regAPW_per_team + 
                           returning_nonconf_regAPW_per_team, 
                         data=vetpred)

summary(returner_APW_model_full)

#Trim model to useful variables. This is our final returner model
returner_APW_model_trim <- lm(reg_adj_allplay_wins ~ 
                                prev_allplay_winpct + 
                                prev_pp_ratio_median + 
                                returning_nonconf_regAPW_per_team, 
                              data=vetpred)

summary(returner_APW_model_trim)

#code check for 2020
franchises20 <- ff_franchises(FAFL20)
