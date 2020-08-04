# Script for creating ADL special Teams Scoring System

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

#Begin by establishing "kickerless" baseline
kickerless <- pbprecent %>% 
  filter(week<=16 &
           (play_type == "pass" | play_type == "run" | play_type == "punt") &  
           !(season == 2014 & (home_team == "JAX" | away_team == "JAX")) &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           down == 4 &
           yardline_100 <= 43 &
           penalty == 0 &
           lateral_reception == 0 & 
           lateral_rush == 0 &
           !is.na(epa))

#Make table for EPA on tries vs. punts.  
kickerless %>% 
  group_by(play_type) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()

#Establish "Kickerless" baseline for Kicker scoring
mean(kickerless$epa)

#See if Kickers add more value at certain distances by checking kickerless EPA at each yard line
grouped_kickerless <- kickerless %>% 
  group_by(yardline_100) %>% 
  summarize(n = n(), mean_epa=mean(epa))

#Plot kickerless EPA by yardline; no pattern
grouped_kickerless %>%
  ggplot(aes(x = yardline_100, y = mean_epa)) +
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(cex=grouped_kickerless$n / 80, color = 'blue', alpha = .6)

fieldgoals <- pbprecent %>% 
  filter(week<=16 &
           (play_type == "field_goal" | field_goal_attempt == 1) &  
           !(season == 2014 & (home_team == "JAX" | away_team == "JAX")) &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           down == 4 &
           penalty == 0 &
           lateral_reception == 0 & 
           lateral_rush == 0 &
           !is.na(epa))
write.csv(fieldgoals, "C:/Users/filim/Documents/R/nflfastR/fieldgoals.csv", row.names = FALSE)
  
fieldgoals %>% 
  group_by(field_goal_result) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()

fgmade <- fieldgoals %>%
  filter(field_goal_result == "made")

fgmissed <- fieldgoals %>%
  filter(field_goal_result == "missed")

grouped_fgmade <- fgmade %>%
  group_by(kick_distance) %>%
  summarize(
    n = n(),
    mean_epa = mean(epa))

grouped_fgmissed <- fgmissed %>%
  group_by(kick_distance) %>%
  summarize(
    n = n(),
    mean_epa = mean(epa))

grouped_fgmade %>%
  ggplot(aes(x = kick_distance, y = mean_epa)) +
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(cex=grouped_fgmade$n / 40, color = 'blue', alpha = .6) +
  geom_point(data=grouped_fgmissed, cex=grouped_fgmissed$n / 40, color = 'red', alpha = .6) +
  #add smooth lines fitting data
  stat_smooth(size = 1, color = 'blue', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  stat_smooth(data=grouped_fgmissed, geom='line', size = 1.2, color = 'red', alpha=0.4, se=FALSE, method='lm') +
  xlim(18,58) +
  ylim(-4,3) +
  #titles and caption
  labs(x = "Kick Distance",
       y = "EPA",
       title = "Field Goals 2014-2019",
       subtitle = "Blue = Made; Red = Missed",
       caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", family="serif"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, family="serif")
  )
#View breakdown of FGs by distance to see which outliers should be removed
view(grouped_fgmade)

#Limit observations to 20-53 yard kicks for fit purposes
limitedfgmade <- fgmade %>%
  filter(kick_distance >= 20 &
           kick_distance <= 53)

#Fit FGs made with a parabolic curve
fgsqfit <- lm(epa ~ kick_distance + I(kick_distance^2), data=limitedfgmade)
summary(fgsqfit)

#Show key benchmarks on curve for Field Goal Distance table
predict(fgsqfit, data.frame(kick_distance = c(20, 25, 30, 35, 40, 45, 50, 55, 60)))

#Look at two point conversion results
twopointconv <- pbprecent %>% 
  filter(week<=16 &
           season >= 2015 &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           penalty == 0 &
           (play_type_nfl == "PAT2" | two_point_attempt == 1) &
           yardline_100 == 2 &
           !is.na(epa))%>%
  mutate(points_scored = score_differential_post - score_differential)
write.csv(twopointconv, "C:/Users/filim/Documents/R/nflfastR/twopointconv.csv", row.names = FALSE)
twopointconv %>% 
  group_by(two_point_conv_result, defensive_two_point_attempt, points_scored) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()

#Look at XP attempt results
extrapoints <- pbprecent %>% 
  filter(week<=16 &
           season >= 2015 &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           penalty == 0 &
           (play_type_nfl == "XP_KICK" | play_type == "extra_point" | extra_point_attempt == 1) &
           yardline_100 == 15 &
           !is.na(epa))%>%
  mutate(points_scored = score_differential_post - score_differential)
write.csv(extrapoints, "C:/Users/filim/Documents/R/nflfastR/extrapoints.csv", row.names = FALSE)

#Create table to assess return rates for One-Point Safety stat
deftwopointatt <- pbprecent %>% 
  filter(week<=16 &
           season >= 2015 &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           penalty == 0 &
           ((yardline_100 == 2 & (play_type_nfl == "PAT2" | two_point_attempt == 1))|
              (yardline_100 == 15 & (play_type_nfl == "XP_KICK" | play_type == "extra_point" | extra_point_attempt == 1))) &
           defensive_two_point_attempt == 1 &
           !is.na(epa))%>%
  mutate(points_scored = score_differential_post - score_differential)

oneptsafety <- deftwopointatt %>% group_by(extra_point_attempt, 
           two_point_attempt,
           defensive_two_point_conv, 
           points_scored) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()
view(oneptsafety)

#Define punterless
punterless <- pbprecent %>% 
  filter(week<=16 &
           (play_type == "pass" | 
              play_type == "run" | 
              play_type == "field_goal" |
              pass_attempt == 1 |
              field_goal_attempt == 1 |
              rush_attempt == 1) &  
           !(season == 2014 & (home_team == "JAX" | away_team == "JAX")) &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           down == 4 &
           penalty == 0 &
           lateral_reception == 0 & 
           lateral_rush == 0 &
           !is.na(epa))

#Define Conversion Try
conversiontry <- pbprecent %>% 
  filter(week<=16 &
           (play_type == "pass" | 
              play_type == "run" | 
              pass_attempt == 1 |
              rush_attempt == 1) &  
           !(season == 2014 & (home_team == "JAX" | away_team == "JAX")) &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           down == 4 &
           penalty == 0 &
           lateral_reception == 0 & 
           lateral_rush == 0 &
           !is.na(epa))

#Filter Punts
punts <- pbprecent %>% 
  filter(week<=16 &
           (play_type == "punt" | punt_attempt == 1) &  
           !(season == 2014 & (home_team == "JAX" | away_team == "JAX")) &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           down == 4 &
           penalty == 0 &
           lateral_reception == 0 & 
           lateral_rush == 0 &
           !is.na(epa))
write.csv(punts, "C:/Users/filim/Documents/R/nflfastR/punts.csv", row.names = FALSE)

grouped_punterless <- punterless %>% 
  group_by(yardline_100) %>% 
  summarize(n = n(), mean_epa=mean(epa), mean_wpa=mean(wpa))

grouped_conversiontry <- conversiontry %>% 
  group_by(yardline_100) %>% 
  summarize(n = n(), mean_epa=mean(epa), mean_wpa=mean(wpa))

grouped_fieldgoals <- fieldgoals %>% 
  group_by(yardline_100) %>% 
  summarize(n = n(), mean_epa=mean(epa), mean_wpa=mean(wpa))

grouped_punts <- punts %>% 
  group_by(yardline_100) %>% 
  summarize(n = n(), mean_epa=mean(epa), mean_wpa=mean(wpa))

#Compare Punt to Punterless
grouped_punts %>%
  ggplot(aes(x = yardline_100, y = mean_epa)) +
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(cex=grouped_punts$n / 150, color = 'blue', alpha = .6) +
  geom_point(data=grouped_punterless, cex=grouped_punterless$n / 150, color = 'red', alpha = .6) +
  #add smooth lines fitting data
  stat_smooth(size = 1, color = 'blue', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  stat_smooth(data=grouped_punterless, size = 1.2, color = 'red', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  xlim(30,100) +
  ylim(-3,2) +
  #titles and caption
  labs(x = "Yards from Opp. End Zone",
       y = "EPA",
       title = "4th Down Plays 2014-2019",
       subtitle = "Blue = Punts; Red = Non-Punts",
       caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", family="serif"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, family="serif")
  )

#Compare all 3 EPA options against one another
grouped_punts %>%
  ggplot(aes(x = yardline_100, y = mean_epa)) +
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(cex=grouped_punts$n / 70, color = 'blue', alpha = .6) +
  geom_point(data=grouped_conversiontry, cex=grouped_conversiontry$n / 70, color = 'red', alpha = .6) +
  geom_point(data=grouped_fieldgoals, cex=grouped_fieldgoals$n / 70, color = 'purple', alpha = .6) +
  #add smooth lines fitting data
  stat_smooth(size = 1, color = 'blue', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  stat_smooth(data=grouped_conversiontry, size = 1.2, color = 'red', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  stat_smooth(data=grouped_fieldgoals, size = 1.2, color = 'purple', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  xlim(25,100) +
  ylim(-3,2) +
  #titles and caption
  labs(x = "Yards from Opp. End Zone",
       y = "EPA",
       title = "4th Down Plays 2014-2019",
       subtitle = "Blue = Punts; Purple = FGs; Red = Conversion Try",
       caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", family="serif"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, family="serif")
  )

#Compare all 3 WPA options against one another
grouped_punts %>%
  ggplot(aes(x = yardline_100, y = mean_wpa)) +
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(cex=grouped_punts$n / 70, color = 'blue', alpha = .6) +
  geom_point(data=grouped_conversiontry, cex=grouped_conversiontry$n / 70, color = 'red', alpha = .6) +
  geom_point(data=grouped_fieldgoals, cex=grouped_fieldgoals$n / 70, color = 'purple', alpha = .6) +
  #add smooth lines fitting data
  stat_smooth(size = 1, color = 'blue', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  stat_smooth(data=grouped_conversiontry, size = 1.2, color = 'red', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  stat_smooth(data=grouped_fieldgoals, size = 1.2, color = 'purple', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  xlim(25,100) +
  ylim(-0.1,0.1) +
  #titles and caption
  labs(x = "Yards from Opp. End Zone",
       y = "WPA",
       title = "4th Down Plays 2014-2019",
       subtitle = "Blue = Punts; Purple = FGs; Red = Conversion Try",
       caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", family="serif"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, family="serif")
  )

view(grouped_fieldgoals)
view(grouped_conversiontry)

smartpunts <- punts %>% 
  filter(yardline_100 >= 43)%>%
  mutate(net_punt_yards = kick_distance - return_yards - 20*touchback)

#Run a "Best Fit" Regression
puntbestfit <- lm(epa ~ kick_distance + punt_inside_twenty + return_yards + return_touchdown + punt_blocked + touchback, data=smartpunts)
summary(puntbestfit)

#Run "Current MFL" Regression
puntmflfit <- lm(epa ~ kick_distance + punt_inside_twenty + punt_blocked, data=smartpunts)
summary(puntmflfit)

#Set up Visual Representation of Touchbacks vs Inside 20 vs Neither

punt_touchbacks <- punts %>%
  filter(touchback == 1)

punts_insidetwenty <- punts %>%
  filter(punt_inside_twenty == 1)

punts_notb_noinside <- punts %>%
  filter(touchback == 0 &
           punt_inside_twenty == 0
         & punt_blocked == 0)

punts_blocked <- punts %>%
  filter(punt_blocked == 1)

grouped_punt_touchbacks <- punt_touchbacks %>%
  group_by(kick_distance) %>% 
  summarize(n = n(), mean_epa=mean(epa))

grouped_punts_insidetwenty <- punts_insidetwenty %>%
  group_by(kick_distance) %>% 
  summarize(n = n(), mean_epa=mean(epa))

grouped_punts_notb_noinside <- punts_notb_noinside %>%
  group_by(kick_distance) %>% 
  summarize(n = n(), mean_epa=mean(epa))

#Compare all 3 EPA options against one another
grouped_punts_notb_noinside %>%
  ggplot(aes(x = kick_distance, y = mean_epa)) +
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(cex=grouped_punts_notb_noinside$n / 70, color = 'black', alpha = .6) +
  geom_point(data=grouped_punt_touchbacks, cex=grouped_punt_touchbacks$n / 70, color = 'red', alpha = .6) +
  geom_point(data=grouped_punts_insidetwenty, cex=grouped_punts_insidetwenty$n / 70, color = 'blue', alpha = .6) +
  #add smooth lines fitting data
  stat_smooth(size = 1, color = 'black', alpha=0.4, se=FALSE, method='lm', formula = y ~ x) +
  stat_smooth(data=grouped_punt_touchbacks, size = 1.2, color = 'red', alpha=0.4, se=FALSE, method='lm', formula = y ~ x) +
  stat_smooth(data=grouped_punts_insidetwenty, size = 1.2, color = 'blue', alpha=0.4, se=FALSE, method='lm', formula = y ~ x) +
  xlim(-0,80) +
  ylim(-3,2.5) +
  #titles and caption
  labs(x = "Punt Yards",
       y = "EPA",
       title = "Unblocked Punts 2014-2019",
       subtitle = "Blue = Inside 20; Red = Touchback; Black = Neither",
       caption = "Data: @nflfastR // Plot: @AnalyticsFF") +
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", family="serif"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, family="serif")
  )
