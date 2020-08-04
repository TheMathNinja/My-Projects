# Script for creating ADL Offensive Scoring System

install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggimage")

install.packages("devtools")
devtools::install_github("mrcaseb/nflfastR")

library(tidyverse)
library(dplyr)
library(na.tools)
library(ggrepel)
library(ggimage)
library(ggplot2)
library(caret)
library(lubridate)
library(nflfastR)

options(scipen = 9999)

# Download new regular season play-by-play files

pbp2019 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
pbp2018 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))
pbp2017 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2017.rds'))
pbp2016 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2016.rds'))
pbp2015 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2015.rds'))
pbp2014 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2014.rds'))

pbprecent <- rbind(pbp2019,pbp2018,pbp2017,pbp2016,pbp2015,pbp2014)

write.csv(pbp2019, "C:/Users/filim/Documents/R/nflfastR/pbp2019.csv", row.names = FALSE)
write.csv(pbp2018, "C:/Users/filim/Documents/R/nflfastR/pbp2018.csv", row.names = FALSE)
write.csv(pbp2017, "C:/Users/filim/Documents/R/nflfastR/pbp2017.csv", row.names = FALSE)
write.csv(pbp2016, "C:/Users/filim/Documents/R/nflfastR/pbp2016.csv", row.names = FALSE)
write.csv(pbp2015, "C:/Users/filim/Documents/R/nflfastR/pbp2015.csv", row.names = FALSE)
write.csv(pbp2014, "C:/Users/filim/Documents/R/nflfastR/pbp2014.csv", row.names = FALSE)
write.csv(pbprecent,"C:/Users/filim/Documents/R/nflfastR/pbprecent.csv", row.names = FALSE)
  
  # All Offensive Play Multiple Linear Regression; excludes fumbles recovered by fumbling player and fumbled interceptions
  # Wondering if we should exclude all defensive fumbles and how to do it.
  # Exclude laterals until/unless we can find a way to credit other players on MFL.
  allOff <- pbprecent %>% 
    filter(week<=16 &
             (play_type == "pass" | play_type == "run") &  
             !(season == 2014 & (home_team == "JAX" | away_team == "JAX")) &
             !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
             penalty == 0 &
             lateral_reception == 0 & 
             lateral_rush == 0 &
             !is.na(epa) &
             !is.na(down))%>%
    mutate(pass_att = pass_attempt - sack)%>%
    mutate(pass_att2 = complete_pass + incomplete_pass + interception)%>%
    mutate(pass_att_check = pass_att - pass_att2)%>%
    mutate(touchdown_off = if_else(touchdown == 1 & td_team == posteam, 1, 0))%>%
    mutate(touchdown_off2 = touchdown - return_touchdown)%>%
    mutate(touchdown_check = touchdown_off - touchdown_off2)%>%
    mutate(rush_pass_touchdown = rush_touchdown + pass_touchdown)%>%
    mutate(rush_yards = if_else(rush_attempt==1, yards_gained, 0))%>%
    mutate(pass_yards = if_else(pass_att==1, yards_gained, 0))%>%
    mutate(sack_yards = if_else(sack==1, -1*yards_gained, 0))%>%
    mutate(yards_check = yards_gained - rush_yards - pass_yards - sack_yards)%>%
    mutate(fumble_1_off = if_else(!is.na(fumbled_1_team) & fumbled_1_team==posteam, 1, 0))%>%
    mutate(fumble_2_off = if_else(!is.na(fumbled_2_team) & fumbled_2_team==posteam, 1, 0))%>%
    mutate(fumbles_off = fumble_1_off + fumble_2_off)%>%
    mutate(own_fumble_recovery_1 = if_else(!is.na(fumbled_1_team) & !is.na(fumble_recovery_1_team) & fumbled_1_team==posteam & fumbled_1_team==fumble_recovery_1_team, 1, 0))%>%
    mutate(own_fumble_recovery_2 = if_else(!is.na(fumbled_2_team) & !is.na(fumble_recovery_2_team) & fumbled_2_team==posteam & fumbled_2_team==fumble_recovery_2_team, 1, 0))%>%
    mutate(own_fumble_recoveries = own_fumble_recovery_1 + own_fumble_recovery_2)%>%
    mutate(off_self_recovery_1 = if_else(!is.na(fumbled_1_team) & !is.na(fumble_recovery_1_team) & fumbled_1_team==posteam & fumbled_1_player_id==fumble_recovery_1_player_id, 1, 0))%>%
    mutate(off_self_recovery_2 = if_else(!is.na(fumbled_2_team) & !is.na(fumble_recovery_2_team) & fumbled_2_team==posteam & fumbled_2_player_id==fumble_recovery_2_player_id, 1, 0))%>%
    mutate(off_self_recoveries = off_self_recovery_1 + off_self_recovery_2)%>%
    mutate(own_fumble_recovery_1_yards = if_else(!is.na(fumbled_1_team) & !is.na(fumble_recovery_1_team) & fumbled_1_team==fumble_recovery_1_team, fumble_recovery_1_yards, 0))%>%
    mutate(own_fumble_recovery_2_yards = if_else(!is.na(fumbled_2_team) & !is.na(fumble_recovery_2_team) & fumbled_2_team==posteam & fumbled_2_team==fumble_recovery_2_team, fumble_recovery_2_yards, 0))%>%
    mutate(own_fumble_recovery_yards = own_fumble_recovery_1_yards + own_fumble_recovery_2_yards)%>%
    mutate(own_fumble_recovery_td_rush = if_else(own_fumble_recoveries >= 1 & touchdown==1 & first_down_rush == 1 & rush_touchdown==0 & pass_touchdown==0, 1, 0))%>%
    mutate(own_fumble_recovery_td_pass = if_else(own_fumble_recoveries >= 1 & touchdown==1 & first_down_pass == 1 & rush_touchdown==0 & pass_touchdown==0, 1, 0))%>%
    mutate(own_fumble_recovery_td = own_fumble_recovery_td_rush + own_fumble_recovery_td_pass)%>%
    mutate(firstdown = first_down-touchdown_off)%>%
    mutate(rush_fd = first_down_rush - rush_touchdown - own_fumble_recovery_td_rush)%>%
    mutate(pass_fd = first_down_pass - pass_touchdown - own_fumble_recovery_td_pass)%>%
    mutate(firstdown_check = firstdown - rush_fd - pass_fd)%>%
    mutate(downs_gained = if_else(first_down == 1, down-1, -1))%>%
    mutate(off_yards = yards_gained + own_fumble_recovery_yards)%>%
    mutate(attempt = rush_attempt + pass_att + sack)
  
  #Quality checks on new variables. pass_att_check, touchdown_check, yards_check, firstdown_check should always be 0. firstdown should only ever be 0 or 1. attempt should always be 1.
  allOff %>% group_by(pass_att_check) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(touchdown_check) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(yards_check) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(firstdown_check) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(firstdown) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(attempt) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(fumbles_off) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(own_fumble_recoveries) %>% summarize(plays = n()) %>% ungroup()
  write.csv(allOff, "C:/Users/filim/Documents/R/nflfastR/allOff.csv", row.names = FALSE)
  
  #Run regression on rushing stats
  rushes <- allOff %>% filter(rush_attempt==1)
  rushes.lm <- lm(epa ~ rush_attempt + pass_att + sack + complete_pass + rush_yards + pass_yards + sack_yards + rush_fd + pass_fd + rush_touchdown + pass_touchdown + interception + fumbles_off + own_fumble_recoveries + own_fumble_recovery_yards + own_fumble_recovery_td + 0, data=rushes)
  summary(rushes.lm)
  write.csv(rushes, "C:/Users/filim/Documents/R/nflfastR/rushes.csv", row.names = FALSE)
  
  #Create passing filter and check there isn't double-counts (allpass==1 only)
  passes <- allOff %>% 
    filter(pass_att==1) %>%
    mutate(allpass = incomplete_pass + complete_pass + interception)
  passes %>% group_by(allpass) %>% summarize(plays = n()) %>% ungroup()
  
  #Run passing regression
  passes.lm <- lm(epa ~ rush_attempt + pass_att + sack + complete_pass + rush_yards + pass_yards + sack_yards + rush_fd + pass_fd + rush_touchdown + pass_touchdown + interception + fumbles_off + own_fumble_recoveries + own_fumble_recovery_yards + own_fumble_recovery_td + 0, data=passes)
  summary(passes.lm)
  write.csv(passes, "C:/Users/filim/Documents/R/nflfastR/passes.csv", row.names = FALSE)
  
  #Test 0 yd Run vs 0 yd Pass v Incomplete Pass v 0 yd Sack for Verifications
  zerorun <- allOff %>%
    filter(rush_attempt==1 &
             fumbles_off == 0 &
             yards_gained==0)
  zeropass <- allOff %>%
    filter(pass_att == 1 &
             yards_gained == 0 &
             interception == 0 &
             fumbles_off == 0 &
             incomplete_pass == 0)
  incomplete <- allOff %>%
    filter(incomplete_pass == 1)
  zerosack <- allOff %>%
    filter(sack == 1 &
             fumbles_off == 0 &
             yards_gained == 0)
  
  write.csv(zerorun, "C:/Users/filim/Documents/R/nflfastR/zerorun.csv", row.names = FALSE)
  write.csv(zeropass, "C:/Users/filim/Documents/R/nflfastR/zeropass.csv", row.names = FALSE)
  write.csv(incomplete, "C:/Users/filim/Documents/R/nflfastR/incomplete.csv", row.names = FALSE)
  write.csv(zerosack, "C:/Users/filim/Documents/R/nflfastR/zerosack.csv", row.names = FALSE)
  
  mean(zerorun$epa)
  mean(zeropass$epa)
  mean(incomplete$epa)
  mean(zerosack$epa)
  
  #Display relationship between rushing yards gained and EPA
  cleanrush <- allOff %>% filter(rush_attempt==1 &
                                  fumbles_off == 0)
  
  baserush <- allOff %>% filter(rush_attempt==1 &
                                  fumbles_off == 0 &
                                  rush_fd == 0 &
                                  rush_touchdown == 0)
  fdrush <- allOff %>% filter(rush_attempt==1 &
                                  fumbles_off == 0 &
                                  rush_fd == 1 &
                                  rush_touchdown == 0)
  rushtd <- allOff %>% filter(rush_attempt==1 &
                                  fumbles_off == 0 &
                                  rush_fd == 0 &
                                  rush_touchdown == 1)
 
   grouped_cleanrush <- cleanrush %>%
    group_by(rush_yards) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  grouped_baserush <- baserush %>%
    group_by(rush_yards) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  grouped_fdrush <- fdrush %>%
    group_by(rush_yards) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  grouped_rushtd <- rushtd %>%
    group_by(rush_yards) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  write.csv(baserush, "C:/Users/filim/Documents/R/nflfastR/baserush.csv", row.names = FALSE)
  
  #Plot all rushes together
  grouped_cleanrush %>%
    ggplot(aes(x = rush_yards, y = mean_epa)) +
    #cex controls point size and alpha the transparency (alpha = 1 is normal)
    geom_point(cex=grouped_cleanrush$n / 4000, alpha = .6) +
    #add a smooth line fitting data
    stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
    #titles and caption
    labs(x = "Rushing Yards",
         y = "EPA per play",
         title = "Rushing Plays",
         caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
    #uses the black and white ggplot theme
    theme_bw() +
    #center title with hjust = 0.5
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
    )
  
  #separate layers for base, first down, and TD rushes
  grouped_baserush %>%
    ggplot(aes(x = rush_yards, y = mean_epa)) +
    #cex controls point size and alpha the transparency (alpha = 1 is normal)
    geom_point(cex=grouped_baserush$n / 4000, color = 'black', alpha = .6) +
    geom_point(data=grouped_fdrush, cex=grouped_fdrush$n / 4000, color = 'blue', alpha = .6) +
    geom_point(data=grouped_rushtd, cex=grouped_rushtd$n / 4000, color = 'red', alpha = .6) +
    #add smooth lines fitting data
    stat_smooth(geom='line', color = 'black', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_fdrush, geom='line', color = 'blue', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_rushtd, geom='line', color = 'red', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_cleanrush, geom='line', color = 'darkorange', alpha=0.5, se=FALSE, method='lm')+
    xlim(-20,100) +
    ylim(-3.5,8) +
    #titles and caption
    labs(x = "Rushing Yards",
         y = "EPA per play",
         title = "Rushing Plays 2014-2019",
         caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
    #uses the black and white ggplot theme
    theme_bw() +
    #center title with hjust = 0.5
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
    )
  
  #Zoom in on key 25 yards
  grouped_baserush %>%
    ggplot(aes(x = rush_yards, y = mean_epa)) +
    #cex controls point size and alpha the transparency (alpha = 1 is normal)
    geom_point(cex=grouped_baserush$n / 2000, color = 'black', alpha = .6) +
    geom_point(data=grouped_fdrush, cex=grouped_fdrush$n / 2000, color = 'blue', alpha = .6) +
    geom_point(data=grouped_rushtd, cex=grouped_rushtd$n / 2000, color = 'red', alpha = .6) +
    #add smooth lines fitting data
    stat_smooth(geom='line', size = 1.2, color = 'black', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_fdrush, geom='line', size = 1.2, color = 'blue', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_rushtd, geom='line', size = 1.2, color = 'red', alpha=0.5, se=FALSE, method='lm') +
    xlim(-5,20) +
    ylim(-1.5,3) +
    #titles and caption
    labs(x = "Rushing Yards",
         y = "EPA per play",
         title = "Rushing Plays 2014-2019",
         caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
    theme_bw() +
    #center title with hjust = 0.5
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", family="serif")
    )
  
  baserush.lm <- lm(epa ~ yards_gained, data=baserush)
  fdrush.lm <- lm(epa ~ yards_gained, data=fdrush)
  rushtd.lm <- lm(epa ~ yards_gained, data=rushtd)
  summary(baserush.lm)
  summary(fdrush.lm)
  summary(rushtd.lm)
  
  #Display relationship between rushing yards gained and EPA
  cleanpass <- allOff %>% filter(pass_att==1 &
                                   fumbles_off == 0 &
                                   interception==0)
  
  basepass <- allOff %>% filter(pass_att==1 &
                                  fumbles_off == 0 &
                                  interception == 0 &
                                  incomplete_pass == 0 &
                                  pass_fd == 0 &
                                  pass_touchdown == 0)
  
  fdpass <- allOff %>% filter(pass_att==1 &
                                fumbles_off == 0 &
                                interception == 0 &
                                pass_fd == 1 &
                                pass_touchdown == 0)
  passtd <- allOff %>% filter(pass_att==1 &
                                fumbles_off == 0 &
                                interception == 0 &
                                pass_fd == 0 &
                                pass_touchdown == 1)
  cleansack <- allOff %>% filter(sack==1 &
                                fumbles_off == 0 &
                                interception == 0)
  
  grouped_cleanpass <- cleanpass %>%
    group_by(yards_gained) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  grouped_basepass <- basepass %>%
    group_by(yards_gained) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  grouped_fdpass <- fdpass %>%
    group_by(yards_gained) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  grouped_passtd <- passtd %>%
    group_by(yards_gained) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  grouped_cleansack <- cleansack %>%
    group_by(yards_gained) %>%
    summarize(
      n = n(),
      mean_epa = mean(epa))
  
  #Plot all passes together
  grouped_cleanpass %>%
    ggplot(aes(x = yards_gained, y = mean_epa)) +
    #cex controls point size and alpha the transparency (alpha = 1 is normal)
    geom_point(cex=grouped_cleanpass$n / 4000, alpha = .6) +
    #add a smooth line fitting data
    stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
    #titles and caption
    labs(x = "Passing Yards",
         y = "EPA per play",
         title = "Passing Plays 2014-2019",
         caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
    #uses the black and white ggplot theme
    theme_bw() +
    #center title with hjust = 0.5
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
    )
  
  #separate layers for sacks, base, first down, and TD passes
  grouped_basepass %>%
    ggplot(aes(x = yards_gained, y = mean_epa)) +
    geom_hline(yintercept = mean(incomplete$epa), color = "red", linetype = "dashed", alpha=0.5) +
    #cex controls point size and alpha the transparency (alpha = 1 is normal)
    geom_point(cex=grouped_basepass$n / 4000, color = 'black', alpha = .6) +
    geom_point(data=grouped_fdpass, cex=grouped_fdpass$n / 4000, color = 'blue', alpha = .6) +
    geom_point(data=grouped_passtd, cex=grouped_passtd$n / 4000, color = 'red', alpha = .6) +
    geom_point(data=grouped_cleansack, cex=grouped_cleansack$n / 4000, color = 'purple', alpha = .6) +
    #add smooth lines fitting data
    stat_smooth(geom='line', color = 'black', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_fdpass, geom='line', color = 'blue', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_passtd, geom='line', color = 'red', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_cleansack, geom='line', color = 'purple', alpha=0.5, se=FALSE, method='lm') +
    stat_smooth(data=grouped_cleanpass, geom='line', color = 'darkorange', alpha=0.5, se=FALSE, method='lm')+
    xlim(-20,100) +
    ylim(-3.5,8) +
    #titles and caption
    labs(x = "Passing Yards",
         y = "EPA per play",
         title = "Passing Plays 2014-2019",
         caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
    #uses the black and white ggplot theme
    theme_bw() +
    #center title with hjust = 0.5
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
    )
  
  #Zoom in on key 30 yards
  grouped_basepass %>%
    ggplot(aes(x = yards_gained, y = mean_epa)) +
    geom_hline(yintercept = mean(incomplete$epa), color = "red", linetype = "dashed", alpha=0.5) +
    #cex controls point size and alpha the transparency (alpha = 1 is normal)
    geom_point(cex=grouped_basepass$n / 700, color = 'black', alpha = .6) +
    geom_point(data=grouped_fdpass, cex=grouped_fdpass$n / 700, color = 'blue', alpha = .6) +
    geom_point(data=grouped_passtd, cex=grouped_passtd$n / 700, color = 'red', alpha = .6) +
    geom_point(data=grouped_cleansack, cex=grouped_cleansack$n / 700, color = 'purple', alpha = .6) +
    #add smooth lines fitting data
    stat_smooth(geom='line', size = 1.2, color = 'black', alpha=0.4, se=FALSE, method='lm') +
    stat_smooth(data=grouped_fdpass, geom='line', size = 1.2, color = 'blue', alpha=0.4, se=FALSE, method='lm') +
    stat_smooth(data=grouped_passtd, geom='line', size = 1.2, color = 'red', alpha=0.4, se=FALSE, method='lm') +
    stat_smooth(data=grouped_cleansack, geom='line', size = 1.2, color = 'purple', alpha=0.4, se=FALSE, method='lm') +
    xlim(-10,20) +
    ylim(-2,3.5) +
    #titles and caption
    labs(x = "Passing Yards",
         y = "EPA per play",
         title = "Passing Plays 2014-2019",
         caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
    theme_bw() +
    #center title with hjust = 0.5
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", family="serif")
    )
  
  basepass.lm <- lm(epa ~ yards_gained, data=basepass)
  fdpass.lm <- lm(epa ~ yards_gained, data=fdpass)
  passtd.lm <- lm(epa ~ yards_gained, data=passtd)
  cleansack.lm <- lm(epa ~ yards_gained, data=cleansack)
  summary(basepass.lm)
  summary(fdpass.lm)
  summary(passtd.lm)
  summary(cleansack.lm)
  
  #Run sacks regression
  sacks <- allOff %>% filter(sack==1)
  sacks.lm <- lm(epa ~ yards_gained + fumble, data=sacks)
  summary(sacks.lm)
  write.csv(sacks, "C:/Users/filim/Documents/R/nflfastR/sacks.csv", row.names = FALSE)
  
  #All Offense Regression
  allOff.lm <- lm(epa ~ rush_attempt + pass_att + sack + complete_pass + rush_yards + pass_yards + sack_yards + rush_fd + pass_fd + rush_touchdown + pass_touchdown + interception + fumbles_off + own_fumble_recoveries + own_fumble_recovery_yards + own_fumble_recovery_td + 0, data=allOff)
  summary(allOff.lm)
  
  #Check extreme residuals for faulty variable reporting 
  residsAll <- resid(allOff.lm)
  qqnorm(residsAll)
  qqline(residsAll)
  alloffbigresids <- allOff %>%
    mutate(error = resid(allOff.lm)) %>%
    filter(error > 5 | error < -5)
  write.csv(alloffbigresids, "C:/Users/filim/Documents/R/nflfastR/bigresids.csv", row.names = FALSE)
  
  #All Offense Consistent Regression
  allOffConsistent.lm <- lm(epa ~ rush_attempt + pass_attempt + complete_pass + off_yards + firstdown + touchdown_off + interception + fumbles_off + own_fumble_recoveries + 0, data=allOff)
  summary(allOffConsistent.lm)
  
  #All Offense Consistent Att vs. FD comparison (no incompletes)
  attvsfd <- allOff %>%
    filter(incomplete_pass==0
           & sack == 0
           & interception == 0)
  allOffConsistent2.lm <- lm(epa ~ attempt + off_yards + firstdown + touchdown_off + fumbles_off + own_fumble_recoveries + 0, data=attvsfd)
  summary(allOffConsistent2.lm)
  allOffConsistent3.lm <- lm(epa ~ rush_attempt + pass_attempt + complete_pass + off_yards + downs_gained + touchdown_off + interception + fumbles_off + own_fumble_recoveries + 0, data=allOff)
  summary(allOffConsistent3.lm)
  
  #Check Average Downs Gained by First Down Plays (both excl. and incl. TDs)
  allOff %>% group_by(down, touchdown_off, firstdown) %>% summarize(plays = n(), epa=mean(epa)) %>% ungroup()
  
  #Analyze all one-offensive-fumble plays
  offensefumbles <- allOff %>% 
    filter(fumble_1_off == 1 & fumble_2_off == 0)%>%
    mutate(self_recovery = if_else(!is.na(fumble_recovery_1_player_id) & fumbled_1_player_id==fumble_recovery_1_player_id, 1, 0))%>%
    mutate(other_off_recovery = if_else(!is.na(fumble_recovery_1_team) & fumble_recovery_1_team==fumbled_1_team & fumbled_1_player_id != fumble_recovery_1_player_id, 1, 0))%>%
    mutate(no_recovery_not_lost = if_else(is.na(fumble_recovery_1_team) & fumble_lost == 0, 1, 0))%>%
    mutate(fumble_check = fumble_1_off - self_recovery - other_off_recovery - no_recovery_not_lost - fumble_lost)
    
  offensefumbles %>% group_by(fumble_check) %>% summarize(plays = n(), yards = mean(yards_gained), EPA = mean(epa)) %>% ungroup()
  offensefumbles %>% group_by(self_recovery) %>% summarize(plays = n(), yards = mean(yards_gained), EPA = mean(epa)) %>% ungroup()
  offensefumbles %>% group_by(other_off_recovery) %>% summarize(plays = n(), yards = mean(yards_gained), EPA = mean(epa)) %>% ungroup()
  offensefumbles %>% group_by(no_recovery_not_lost) %>% summarize(plays = n(), yards = mean(yards_gained), EPA = mean(epa)) %>% ungroup()
  offensefumbles %>% group_by(fumble_lost) %>% summarize(plays = n(), yards = mean(yards_gained), EPA = mean(epa)) %>% ungroup()
  
  write.csv(offensefumbles, "C:/Users/filim/Documents/R/nflfastR/offensefumbles.csv", row.names = FALSE)
  
  #Analyze Multi-Fumble Plays
  multifumble <- allOff %>% 
    filter(!is.na(fumbled_1_team) & !is.na(fumbled_2_team))
  write.csv(multifumble, "C:/Users/filim/Documents/R/nflfastR/multifumble.csv", row.names = FALSE)
  
  #Final Regression requires we refine coefficient on Fumble by accounting for self-recoveries; removes recovery TDs from TD stat cuz that's another player
  finaloffreg.lm <- lm(epa ~ rush_attempt + pass_attempt + complete_pass + yards_gained + firstdown + rush_pass_touchdown + interception + fumbles_off + off_self_recoveries + 0, data=allOff)
  summary(finaloffreg.lm)
  
  #Introduce Penalties to see their impact
  allOffwithpen <- pbprecent %>% 
    filter(week<=16 &
             (play_type == "pass" | play_type == "run" | play_type == "no_play") & 
             !(play_type_nfl == "FIELD_GOAL" | play_type_nfl == "PUNT") &
             !(season == 2014 & (home_team == "JAX" | away_team == "JAX")) &
             !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
             lateral_reception == 0 & 
             lateral_rush == 0 &
             !(penalty == 1 & penalty_team == defteam) &
             !is.na(epa) &
             !is.na(down))%>%
    mutate(pass_att = pass_attempt - sack)%>%
    mutate(touchdown_off = if_else(touchdown == 1 & td_team == posteam, 1, 0))%>%
    mutate(touchdown_off2 = touchdown - return_touchdown)%>%
    mutate(touchdown_check = touchdown_off - touchdown_off2)%>%
    mutate(rush_pass_touchdown = rush_touchdown + pass_touchdown)%>%
    mutate(rush_yards = if_else(rush_attempt==1, yards_gained, 0))%>%
    mutate(pass_yards = if_else(pass_att==1, yards_gained, 0))%>%
    mutate(sack_yards = if_else(sack==1, yards_gained, 0))%>%
    mutate(yards_check = yards_gained - rush_yards - pass_yards - sack_yards)%>%
    mutate(fumble_1_off = if_else(!is.na(fumbled_1_team) & fumbled_1_team==posteam, 1, 0))%>%
    mutate(fumble_2_off = if_else(!is.na(fumbled_2_team) & fumbled_2_team==posteam, 1, 0))%>%
    mutate(fumbles_off = fumble_1_off + fumble_2_off)%>%
    mutate(own_fumble_recovery_1 = if_else(!is.na(fumbled_1_team) & !is.na(fumble_recovery_1_team) & fumbled_1_team==posteam & fumbled_1_team==fumble_recovery_1_team, 1, 0))%>%
    mutate(own_fumble_recovery_2 = if_else(!is.na(fumbled_2_team) & !is.na(fumble_recovery_2_team) & fumbled_2_team==posteam & fumbled_2_team==fumble_recovery_2_team, 1, 0))%>%
    mutate(own_fumble_recoveries = own_fumble_recovery_1 + own_fumble_recovery_2)%>%
    mutate(off_self_recovery_1 = if_else(!is.na(fumbled_1_team) & !is.na(fumble_recovery_1_team) & fumbled_1_team==posteam & fumbled_1_player_id==fumble_recovery_1_player_id, 1, 0))%>%
    mutate(off_self_recovery_2 = if_else(!is.na(fumbled_2_team) & !is.na(fumble_recovery_2_team) & fumbled_2_team==posteam & fumbled_2_player_id==fumble_recovery_2_player_id, 1, 0))%>%
    mutate(off_self_recoveries = off_self_recovery_1 + off_self_recovery_2)%>%
    mutate(own_fumble_recovery_1_yards = if_else(!is.na(fumbled_1_team) & !is.na(fumble_recovery_1_team) & fumbled_1_team==fumble_recovery_1_team, fumble_recovery_1_yards, 0))%>%
    mutate(own_fumble_recovery_2_yards = if_else(!is.na(fumbled_2_team) & !is.na(fumble_recovery_2_team) & fumbled_2_team==posteam & fumbled_2_team==fumble_recovery_2_team, fumble_recovery_2_yards, 0))%>%
    mutate(own_fumble_recovery_yards = own_fumble_recovery_1_yards + own_fumble_recovery_2_yards)%>%
    mutate(own_fumble_recovery_td_rush = if_else(own_fumble_recoveries >= 1 & touchdown==1 & first_down_rush == 1 & rush_touchdown==0 & pass_touchdown==0, 1, 0))%>%
    mutate(own_fumble_recovery_td_pass = if_else(own_fumble_recoveries >= 1 & touchdown==1 & first_down_pass == 1 & rush_touchdown==0 & pass_touchdown==0, 1, 0))%>%
    mutate(own_fumble_recovery_td = own_fumble_recovery_td_rush + own_fumble_recovery_td_pass)%>%
    mutate(firstdown = first_down-touchdown_off)%>%
    mutate(rush_fd = first_down_rush - rush_touchdown - own_fumble_recovery_td_rush)%>%
    mutate(pass_fd = first_down_pass - pass_touchdown - own_fumble_recovery_td_pass)%>%
    mutate(firstdown_check = firstdown - rush_fd - pass_fd)%>%
    mutate(downs_gained = if_else(first_down == 1, down-1, -1))%>%
    mutate(adj_penalty_yards = if_else(is.na(penalty_yards), 0, penalty_yards))%>%
    mutate(off_yards = yards_gained + own_fumble_recovery_yards)%>%
    mutate(attempt = rush_attempt + pass_att + sack)
  
  #Quality checks on new variables. touchdown_check, yards_check, firstdown_check should always be 0. firstdown should only ever be 0 or 1. attempt should always be 1.
  allOff %>% group_by(touchdown_check) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(yards_check) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(firstdown_check) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(firstdown) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(attempt) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(fumbles_off) %>% summarize(plays = n()) %>% ungroup()
  allOff %>% group_by(own_fumble_recoveries) %>% summarize(plays = n()) %>% ungroup()
  write.csv(allOffwithpen, "C:/Users/filim/Documents/R/nflfastR/allOffwithpen.csv", row.names = FALSE)
  
  #Redo final regression now including offensive penalties (run second regression for penalty yards only)
  finaloffregwithpen1.lm <- lm(epa ~ rush_attempt + pass_attempt + complete_pass + yards_gained + firstdown + rush_pass_touchdown + interception + fumbles_off + off_self_recoveries + penalty + adj_penalty_yards + 0, data=allOffwithpen)
  summary(finaloffregwithpen1.lm)
  
  finaloffregwithpen2.lm <- lm(epa ~ rush_attempt + pass_attempt + complete_pass + yards_gained + firstdown + rush_pass_touchdown + interception + fumbles_off + off_self_recoveries + adj_penalty_yards + 0, data=allOffwithpen)
  summary(finaloffregwithpen2.lm)
  
  #Illustrate negative impact of Penalty variable via graph of EPA vs. Penalty Yards when there are 0 attempts.
  noplaypenalties <- allOffwithpen %>%
    filter(attempt == 0 &
             penalty == 1)
  write.csv(noplaypenalties, "C:/Users/filim/Documents/R/nflfastR/noplaypenalties.csv", row.names = FALSE)
  
  noplaypenalties %>% 
    group_by(adj_penalty_yards) %>% 
    summarize(n = n(), mean_epa = mean(epa)) %>%
  ungroup()
  
  grouped_noplaypenalties <- noplaypenalties %>% 
    group_by(adj_penalty_yards) %>% 
    summarize(n = n(), mean_epa = mean(epa))
  
  #Plot all passes together
  grouped_noplaypenalties %>%
    ggplot(aes(x = adj_penalty_yards, y = mean_epa)) +
    #cex controls point size and alpha the transparency (alpha = 1 is normal)
    geom_point(cex=grouped_noplaypenalties$n / 1000, alpha = .6) +
    #add a smooth line fitting data
    stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
    #titles and caption
    labs(x = "Penalty Yards",
         y = "EPA per play",
         title = "No-Play Penalties 2014-2019",
         caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
    #uses the black and white ggplot theme
    theme_bw() +
    #center title with hjust = 0.5
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
    )
  
  #Understand how fumble recovery yardage works in dataset
  ownfumblerecoveries <- allOff %>% 
    filter(own_fumble_recoveries>=1)
  
  fumbleyardage <- ownfumblerecoveries %>%
    filter(fumbled_1_player_name != fumble_recovery_1_player_name)
  
  #To determine how first downs are credited on fumble plays. Looks like whoever crosses the "first down marker" gets credited with the rush/pass first down.
  #No such thing as "recovery" first down, even though there are recovery TDs.
  fumblefirstdown <- ownfumblerecoveries %>%
    filter(fumbled_1_player_name != fumble_recovery_1_player_name &
             first_down == 1 &
             touchdown == 0 &
             yards_gained <= ydstogo)
  
  write.csv(fumbles2019, "C:/Users/filim/Documents/R/nflfastR/fumbles2019.csv", row.names = FALSE)
  write.csv(ownfumblerecoveries, "C:/Users/filim/Documents/R/nflfastR/ownfumblerecoveries.csv", row.names = FALSE)
  write.csv(fumbleyardage, "C:/Users/filim/Documents/R/nflfastR/fumbleyardage.csv", row.names = FALSE)
  write.csv(fumblefirstdown, "C:/Users/filim/Documents/R/nflfastR/fumblefirstdown.csv", row.names = FALSE)
  
 
  #Investigations into special games and plays to figure out stat definitions
    laterals <- allOff %>% 
    filter(lateral_reception==1 | lateral_rush==1 | lateral_recovery==1)
  write.csv(laterals, "C:/Users/filim/Documents/R/nflfastR/laterals.csv", row.names = FALSE)
  
  houno <- allOff %>% 
    filter(game_id=="2019_01_HOU_NO")
  write.csv(houno, "C:/Users/filim/Documents/R/nflfastR/houno.csv", row.names = FALSE)
  
  nejax <- allOff %>% 
    filter(game_id=="2018_02_NE_JAX")
  write.csv(nejax, "C:/Users/filim/Documents/R/nflfastR/nejax.csv", row.names = FALSE)
  
  miaoak <- allOff %>% 
    filter(game_id=="2017_09_OAK_MIA" & sack==1)
  write.csv(miaoak, "C:/Users/filim/Documents/R/nflfastR/miaoak.csv", row.names = FALSE)
  
  