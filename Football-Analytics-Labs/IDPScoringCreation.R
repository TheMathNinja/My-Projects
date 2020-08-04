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

#Create IDP variables in whole dataset
alldef <- pbprecent %>% 
  filter(week<=16 &
           (play_type == "pass" | play_type == "run") &  
           !(season == 2014 & (home_team == "JAX" | away_team == "JAX")) &
           !(season == 2015 & (home_team == "JAX" | away_team == "JAX")) &
           penalty == 0 &
           lateral_reception == 0 & 
           lateral_rush == 0 &
           !is.na(epa) &
           !is.na(down))%>%
  mutate(minus_epa = -1*epa)%>%
  mutate(qb_hit_1 = if_else(!is.na(qb_hit_1_player_id),1,0))%>%
  mutate(qb_hit_2 = if_else(!is.na(qb_hit_2_player_id),1,0))%>%
  mutate(qb_hits = qb_hit_1 + qb_hit_2)%>%
  mutate(fumble_1_off = if_else(!is.na(fumbled_1_team) & fumbled_1_team==posteam, 1, 0))%>%
  mutate(fumble_2_off = if_else(!is.na(fumbled_2_team) & fumbled_2_team==posteam, 1, 0))%>%
  mutate(fumbles_off = fumble_1_off + fumble_2_off)%>%
  mutate(idp_solo_1 = if_else(!is.na(solo_tackle_1_player_id) & solo_tackle_1_team == defteam,1,0))%>%
  mutate(idp_solo_2 = if_else(!is.na(solo_tackle_2_player_id) & solo_tackle_2_team == defteam,1,0))%>%
  mutate(idp_solos = idp_solo_1 + idp_solo_2)%>%
  mutate(idp_assist_1 = if_else(!is.na(assist_tackle_1_player_id) & assist_tackle_1_team == defteam,1,0))%>%
  mutate(idp_assist_2 = if_else(!is.na(assist_tackle_2_player_id) & assist_tackle_2_team == defteam,1,0))%>%
  mutate(idp_assist_3 = if_else(!is.na(assist_tackle_3_player_id) & assist_tackle_3_team == defteam,1,0))%>%
  mutate(idp_assist_4 = if_else(!is.na(assist_tackle_4_player_id) & assist_tackle_4_team == defteam,1,0))%>%
  mutate(idp_assists = idp_assist_1 + idp_assist_2 + idp_assist_3 + idp_assist_4)%>%
  mutate(total_tackles = idp_solos + idp_assists)%>%
  mutate(tackle_for_loss_1 = if_else(!is.na(tackle_for_loss_1_player_id),1,0))%>%
  mutate(tackle_for_loss_2 = if_else(!is.na(tackle_for_loss_2_player_id),1,0))%>%
  mutate(tackles_for_loss = tackle_for_loss_1 + tackle_for_loss_2)%>%
  mutate(sack_yards = if_else(sack==1, -1*yards_gained, 0))%>%
  mutate(idp_ff_1 = if_else(!is.na(forced_fumble_player_1_player_id) & forced_fumble_player_1_team == defteam,1,0))%>%
  mutate(idp_ff_2 = if_else(!is.na(forced_fumble_player_2_player_id) & forced_fumble_player_2_team == defteam,1,0))%>%
  mutate(idp_forced_fumbles = idp_ff_1 + idp_ff_2)%>%
  mutate(idp_opp_fr_1 = if_else(!is.na(fumble_recovery_1_player_id) & fumble_recovery_1_team == defteam & fumbled_1_team == posteam,1,0))%>%
  mutate(idp_opp_fr_2 = if_else(!is.na(fumble_recovery_2_player_id) & fumble_recovery_2_team == defteam & fumbled_2_team == posteam,1,0))%>%
  mutate(idp_opp_fumble_recoveries = idp_opp_fr_1 + idp_opp_fr_2)%>%
  mutate(idp_opp_fr_yards_1 = if_else(!is.na(fumble_recovery_1_player_id) & fumble_recovery_1_team == defteam & fumbled_1_team == posteam, fumble_recovery_1_yards,0))%>%
  mutate(idp_opp_fr_yards_2 = if_else(!is.na(fumble_recovery_2_player_id) & fumble_recovery_2_team == defteam  & fumbled_2_team == posteam, fumble_recovery_2_yards,0))%>%
  mutate(idp_opp_fumble_recovery_yards = idp_opp_fr_yards_1 + idp_opp_fr_yards_2)%>%
  mutate(idp_opp_fumble_recovery_tds = if_else(idp_opp_fumble_recoveries >= 1,return_touchdown, 0))%>%
  mutate(idp_own_fr_1 = if_else(!is.na(fumble_recovery_1_player_id) & fumble_recovery_1_team == defteam & fumbled_1_team == defteam,1,0))%>%
  mutate(idp_own_fr_2 = if_else(!is.na(fumble_recovery_2_player_id) & fumble_recovery_2_team == defteam  & fumbled_2_team == defteam,1,0))%>%
  mutate(idp_own_fumble_recoveries = idp_own_fr_1 + idp_own_fr_2)%>%
  mutate(idp_own_fr_yards_1 = if_else(!is.na(fumble_recovery_1_player_id) & fumble_recovery_1_team == defteam & fumbled_1_team == defteam, fumble_recovery_1_yards,0))%>%
  mutate(idp_own_fr_yards_2 = if_else(!is.na(fumble_recovery_2_player_id) & fumble_recovery_2_team == defteam  & fumbled_2_team == defteam, fumble_recovery_2_yards,0))%>%
  mutate(idp_own_fumble_recovery_yards = idp_own_fr_yards_1 + idp_own_fr_yards_2)%>%
  mutate(idp_own_fumble_recovery_tds = if_else(idp_own_fumble_recoveries >= 1,return_touchdown, 0))%>%
  mutate(pass_defensed_1 = if_else(!is.na(pass_defense_1_player_id),1,0))%>%
  mutate(pass_defensed_2 = if_else(!is.na(pass_defense_2_player_id),1,0))%>%
  mutate(passes_defensed = pass_defensed_1 + pass_defensed_2)%>%
  mutate(pd_minus_int = passes_defensed - interception)%>%
  mutate(int_return_yards = if_else(interception==1, return_yards, 0))%>%
  mutate(int_return_td = if_else(interception==1, return_touchdown, 0))%>%
  mutate(idp_return_yards = idp_opp_fumble_recovery_yards + int_return_yards + idp_own_fumble_recovery_yards)%>%
  mutate(idp_return_td = idp_opp_fumble_recovery_tds + int_return_yards + idp_own_fumble_recovery_tds)%>%
  mutate(idp_fumble_1 = if_else(!is.na(fumbled_1_player_id) & fumbled_1_team == defteam, 1,0))%>%
  mutate(idp_fumble_2 = if_else(!is.na(fumbled_2_player_id) & fumbled_2_team == defteam, 1,0))%>%
  mutate(idp_fumbles = idp_fumble_1 + idp_fumble_2)%>%
  mutate(pass_att = pass_attempt - sack)%>%
  mutate(int_player_pd_1 = if_else(!is.na(interception_player_id) & !is.na(pass_defense_1_player_id) & interception_player_id == pass_defense_1_player_id, 1,0))%>%
  mutate(int_player_pd_2 = if_else(!is.na(interception_player_id) & !is.na(pass_defense_2_player_id) & interception_player_id == pass_defense_2_player_id, 1,0))%>%
  mutate(int_player_pd = int_player_pd_1 + int_player_pd_2)%>%
  mutate(neg_epa = if_else(epa < 0, 1, 0))

#Run a couple stat checks (eventually include TFG >= 0 once Tackle variable is created)
alldef %>% group_by(pd_minus_int) %>% summarize(plays = n()) %>% ungroup()

#Read in PFR Player Data for First Down Prediction Model
pfr_rec_2019 <- read_csv("C:/Users/filim/Documents/R/PFRPlayerData/ReceivingStats2019.csv")
pfr_rec_2018 <- read_csv("C:/Users/filim/Documents/R/PFRPlayerData/ReceivingStats2018.csv")
pfr_rec_2017 <- read_csv("C:/Users/filim/Documents/R/PFRPlayerData/ReceivingStats2017.csv")
pfr_rec_2016 <- read_csv("C:/Users/filim/Documents/R/PFRPlayerData/ReceivingStats2016.csv")

#Rename troublesome columns and create new variables
pfr_rec_recent <- rbind(pfr_rec_2019,pfr_rec_2018,pfr_rec_2017,pfr_rec_2016)%>%
  rename(
    catch_pct = `Ctch%`,
    ypr = `Y/R`,
    ypt = `Y/Tgt`,
    recpg = `R/G`,
    ypg = `Y/G`
  ) %>%
  mutate(recfdpct = 100*FD/Rec)%>%
  mutate(recfdnotdpct = 100*(FD-TD)/Rec)%>%
  mutate(yards_per_rec = Yds/Rec)

rec_fd_model_data <- pfr_rec_recent %>%
  filter(Rec > 20 & 
           FD > 0 &
           recfdpct > 0 &
           (Pos == "WR" & ypg >= 35) |
           (Pos == "RB" & ypg >= 15) |
           (Pos == "TE" & ypg >= 15))

view(rec_fd_model_data)

rec_fd_model_data %>%
  ggplot(aes(x = yards_per_rec, y = recfdpct, color = Pos)) +
  #titles and caption
  geom_point(alpha = .6) +
  stat_smooth(size = 1, color = 'purple', alpha=0.4, se=FALSE, method='lm', formula = y ~ x + I(x^2)) +
  labs(x = "Yards Per Rec",
       y = "First Down %",
       title = "All Receivers, 2016-2019",
       caption = "Data: @ProFootballReference // Plot: @AnalyticsFF") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )

recfdsqmodel <- lm(recfdpct ~ yards_per_rec + I(yards_per_rec^2), data = )

#Read in PFF Elite Data
pff_defsum2017 <- read_csv("C:/Users/filim/Documents/R/PFFElite/2017/Defense/defense_summary.csv")%>%
  mutate(year = 2017)
pff_defsum2016 <- read_csv("C:/Users/filim/Documents/R/PFFElite/2016/Defense/defense_summary.csv")%>%
  mutate(year = 2016)
pff_defsum2015 <- read_csv("C:/Users/filim/Documents/R/PFFElite/2015/Defense/defense_summary.csv")%>%
  mutate(year = 2015)
pff_defsum2014 <- read_csv("C:/Users/filim/Documents/R/PFFElite/2014/Defense/defense_summary.csv")%>%
  mutate(year = 2014)

pff_idp_recent <- rbind(pff_defsum2017,pff_defsum2016,pff_defsum2015,pff_defsum2014)%>%
  filter(snap_counts_total >= 2 & 
           (position == "DI" |
              position == "ED" |
              position == "LB" |
              position == "CB" |
              position == "S"))%>%
  mutate(batted_plus_breakups = batted_passes + pass_break_ups)%>%
  mutate(untargeted_cov_snaps = snap_counts_coverage - targets)%>%
  mutate(snaps_per_game = snap_counts_total/player_game_count)%>%
  mutate(air_yards_against = yards - yards_after_catch)%>%
  mutate(air_yards_per_rec = air_yards_against/receptions)%>%
  mutate(fal_allowed_cov = -1*targets + 0.65*receptions + 0.2*yards + 4.5*touchdowns)%>%
  mutate(target_rate = targets/snap_counts_coverage)%>%
  mutate(pff_cov_grade_pts = (grades_coverage_defense - 55)*snap_counts_coverage/100)%>%
  mutate(pff_tfl_proxy = 0.25*(stops-batted_plus_breakups-interceptions))%>%
  mutate(pff_tfg_proxy = tackles - pff_tfl_proxy)%>%
  mutate(pff_pd_proxy = batted_plus_breakups + interceptions)

#Separate by position
pffdatadi <-  pff_idp_recent %>%
  filter(position == "DI")
pffdataed <-  pff_idp_recent %>%
  filter(position == "ED")
pffdatalb <-  pff_idp_recent %>%
  filter(position == "LB")
pffdatacb <-  pff_idp_recent %>%
  filter(position == "CB")
pffdatas <-  pff_idp_recent %>%
  filter(position == "S")

#Now filter out relevant players
rel_pffdatadi <-  pff_idp_recent %>%
  filter(position == "DI" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdataed <-  pff_idp_recent %>%
  filter(position == "ED" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdatalb <-  pff_idp_recent %>%
  filter(position == "LB" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdatacb <-  pff_idp_recent %>%
  filter(position == "CB" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdatas <-  pff_idp_recent %>%
  filter(position == "S" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdatadb <-  pff_idp_recent %>%
  filter((position == "S" | position == "CB") & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)

#Establish baseline by finding average EPA of defensive plays with no IDP stats
noidpstat <- alldef %>% 
  filter(idp_solos == 0 & 
           idp_assists == 0 &  
           tackles_for_loss == 0 &  
           qb_hits == 0 &  
           sack == 0 &  
           sack_yards == 0 &  
           safety == 0 &  
           idp_forced_fumbles == 0 &  
           idp_opp_fumble_recoveries == 0 & 
           idp_opp_fumble_recovery_yards == 0 & 
           idp_opp_fumble_recovery_tds == 0 & 
           pd_minus_int == 0 & 
           interception == 0 & 
           int_return_yards == 0 &  
           int_return_td == 0 & 
           idp_fumbles == 0 & 
           idp_own_fumble_recoveries == 0 & 
           idp_own_fumble_recovery_yards == 0 & 
           idp_own_fumble_recovery_tds == 0)
  mean(noidpstat$epa)
  
  noidpstatrun <- noidpstat%>% 
    filter(rush_attempt == 1)
  mean(noidpstatrun$epa)
  
  noidpstatpass <- noidpstat%>%
    filter(pass_attempt == 1)
  mean(noidpstatpass$epa)
  
  #Check properties of nflfastR data set
  alldef %>% 
    group_by(tackles_for_loss, idp_solos, idp_assists) %>% 
    summarize(n = n(), epa=mean(epa)) %>% ungroup()
  
  tdanalysis <- alldef %>% 
    group_by(interception, idp_fumbles, return_touchdown, touchdown) %>% 
    summarize(n = n(), epa=mean(epa)) %>% ungroup()
  view(tdanalysis)

  twosolos <- alldef %>%
    filter(idp_solos == 2)
  write.csv(twosolos, "C:/Users/filim/Documents/R/nflfastR/twosolos.csv", row.names = FALSE)
  
  tfls <- alldef %>%
    filter(tackles_for_loss >= 1)
  write.csv(tfls, "C:/Users/filim/Documents/R/nflfastR/tfls.csv", row.names = FALSE)
  tflnosackhitff <- alldef %>%
    filter(tackles_for_loss >= 1 &
             sack == 0 &
             idp_forced_fumbles == 0 &
             qb_hits == 0)
  mean(tflnosackhitff$epa)
  
#Study how fumble recovery yardage is recorded on FR plays
  idpfumblerecoveries <- alldef %>%
    filter(idp_opp_fumble_recoveries + idp_own_fumble_recoveries >= 1)
  write.csv(idpfumblerecoveries, "C:/Users/filim/Documents/R/nflfastR/idpfumblerecoveries.csv", row.names = FALSE)
  
#use tackled_for_loss variable combined with assists to look at assisted TFLs? tackled + pass plays neg + sacks?
#look at blocked XP, FG, Punts from IDP perspective
#separate pass-play analysis (pressures, hits, sacks, PDs, INTs, coverage) from generic analysis (FF, FR, TFL, Solo, Asst)
  #Incorporate Sam Hoppen's Position Designations

#Define Separate Baselines for Pass and Run Plays
  passplaysidp <- alldef %>%
    filter(pass_attempt == 1)
  
  runplaysidp <- alldef %>%
    filter(rush_attempt == 1)
 
  mean(passplaysidp$epa)
  mean(runplaysidp$epa)
  
negepa <- alldef %>%
  filter(epa < 0)

idpstatnegepa <- negepa %>%
  filter(!
           (idp_solos == 0 & 
              idp_assists == 0 &  
              tackles_for_loss == 0 &  
              qb_hits == 0 &  
              sack == 0 &  
              sack_yards == 0 &  
              safety == 0 &  
              idp_forced_fumbles == 0 &  
              idp_opp_fumble_recoveries == 0 & 
              idp_opp_fumble_recovery_yards == 0 & 
              idp_opp_fumble_recovery_tds == 0 & 
              pd_minus_int == 0 & 
              interception == 0 & 
              int_return_yards == 0 &  
              int_return_td == 0 & 
              idp_fumbles == 0 & 
              idp_own_fumble_recoveries == 0 & 
              idp_own_fumble_recovery_yards == 0 & 
              idp_own_fumble_recovery_tds == 0))

#Run initial Regression
idpregall <- lm(minus_epa ~ 
                  idp_solos + 
                  idp_assists + 
                  tackles_for_loss + 
                  qb_hits + 
                  sack + 
                  sack_yards + 
                  safety + 
                  idp_forced_fumbles + 
                  idp_opp_fumble_recoveries +
                  idp_opp_fumble_recovery_yards +
                  idp_opp_fumble_recovery_tds +
                  pd_minus_int +
                  interception +
                  int_return_yards + 
                  int_return_td +
                  idp_fumbles +
                  idp_own_fumble_recoveries +
                  idp_own_fumble_recovery_yards +
                  idp_own_fumble_recovery_tds,
                data=alldef)
summary(idpregall)

#Now run a regression which separates pass from run plays for EPA context
idpregcontext <- lm(minus_epa ~ 
                  rush_attempt +
                  pass_attempt +
                  idp_solos + 
                  idp_assists + 
                  tackles_for_loss + 
                  qb_hits + 
                  sack + 
                  sack_yards + 
                  safety + 
                  idp_forced_fumbles + 
                  idp_opp_fumble_recoveries +
                  idp_opp_fumble_recovery_yards +
                  idp_opp_fumble_recovery_tds +
                  pd_minus_int +
                  interception +
                  int_return_yards + 
                  int_return_td +
                  idp_fumbles +
                  idp_own_fumble_recoveries +
                  idp_own_fumble_recovery_yards +
                  idp_own_fumble_recovery_tds +
                  0,
                data=alldef)
summary(idpregcontext)

#Now run the regression on minus EPA plays only
idpregneg <- lm(minus_epa ~ 
                  idp_solos + 
                  idp_assists + 
                  tackles_for_loss + 
                  qb_hits + 
                  sack + 
                  sack_yards + 
                  safety + 
                  idp_forced_fumbles + 
                  idp_opp_fumble_recoveries +
                  idp_opp_fumble_recovery_yards +
                  idp_opp_fumble_recovery_tds +
                  pd_minus_int +
                  interception +
                  int_return_yards + 
                  int_return_td +
                  idp_fumbles +
                  idp_own_fumble_recoveries +
                  idp_own_fumble_recovery_yards +
                  idp_own_fumble_recovery_tds,
                data=negepa)
summary(idpregneg)

#Now run the regression only on minus EPA plays with some kind of IDP stat
idpregneg2 <- lm(minus_epa ~ 
                  idp_solos + 
                  idp_assists + 
                  tackles_for_loss + 
                  qb_hits + 
                  sack + 
                  sack_yards + 
                  safety + 
                  idp_forced_fumbles + 
                  idp_opp_fumble_recoveries +
                  idp_opp_fumble_recovery_yards +
                  idp_opp_fumble_recovery_tds +
                  pd_minus_int +
                  interception +
                  int_return_yards + 
                  int_return_td +
                  idp_fumbles +
                  idp_own_fumble_recoveries +
                  idp_own_fumble_recovery_yards +
                  idp_own_fumble_recovery_tds + 
                   0,
                data=idpstatnegepa)
summary(idpregneg2)

#Now simplify by combining IDP return yards and TDs, removing Safeties and FFs
idpregneg3 <- lm(minus_epa ~ 
                   idp_solos + 
                   idp_assists + 
                   tackles_for_loss + 
                   qb_hits + 
                   sack + 
                   sack_yards + 
                   idp_opp_fumble_recoveries +
                   pd_minus_int +
                   interception +
                   idp_fumbles +
                   idp_own_fumble_recoveries +
                   idp_return_yards +
                   idp_return_td +
                   0,
                 data=idpstatnegepa)
summary(idpregneg3)

#see if sacks are TFLs and QBHits every time
sacks <- alldef %>% 
  filter(sack == 1)
write.csv(sacks, "C:/Users/filim/Documents/R/nflfastR/sacks.csv", row.names = FALSE)
sacknoff <- sacks %>%
  filter(idp_forced_fumbles == 0)
mean(sacknoff$sack_yards)

#Something we see below is that TFL gets a bad coefficient here 
#because it often signals the Off recovered the forced fumble (then TFL by IDP)
sackanalysis <- sacks %>% 
  group_by(tackles_for_loss, qb_hits, idp_forced_fumbles) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()
view(sackanalysis)

hitanalysis <- passplaysidp %>% 
  group_by(sack, qb_hit, idp_ff_1) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()
view(hitanalysis)

#Simplify regression by taking out FR yards and TDs and forced fumbles.
passplayreg <- lm(minus_epa ~ 
                    tackles_for_loss + 
                    qb_hits + 
                    sack + 
                    sack_yards + 
                    safety + 
                    idp_forced_fumbles +
                    pd_minus_int +
                    interception +
                    int_return_yards + 
                    int_return_td,
                  data=passplaysidp)
summary(passplayreg)

#Check for cascade effect of QB Hits
qbhcascade <- lm(minus_epa ~ 
                    tackles_for_loss + 
                    qb_hits + 
                    sack + 
                    sack_yards + 
                    safety + 
                    idp_forced_fumbles, 
                  data=passplaysidp)
summary(qbhcascade)

thrownball <- alldef %>% 
  filter(pass_att == 1)

qbhpasscascade <- thrownball %>% 
  group_by(qb_hits, complete_pass, interception) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()
view(qbhpasscascade)

#Do QB Hurry Analysis here with PFF Data
dihurries <- lm(hurries ~ tackles + assists + pff_tfl_proxy + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdatadi)
summary(dihurries)
#Refine DI Hurry Model
dihurries <- lm(hurries ~ tackles + sacks + hits + 0, data=rel_pffdatadi)
summary(dihurries)
#ED Model
edhurries <- lm(hurries ~ tackles + assists + pff_tfl_proxy + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdataed)
summary(edhurries)
#Refine ED Hurry Model
edhurries <- lm(hurries ~ pff_tfl_proxy + sacks + hits + batted_plus_breakups + 0, data=rel_pffdataed)
summary(edhurries)
#LB Model
lbhurries <- lm(hurries ~ tackles + assists + pff_tfl_proxy + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdatalb)
summary(lbhurries)
#Refine LB Hurry Model
lbhurries <- lm(hurries ~ tackles + pff_tfl_proxy + sacks + hits + batted_plus_breakups + 0, data=rel_pffdatalb)
summary(lbhurries)
#CB Model
cbhurries <- lm(hurries ~ tackles + assists + pff_tfl_proxy + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdatacb)
summary(cbhurries)
#Refine CB Hurry Model
cbhurries <- lm(hurries ~ pff_tfl_proxy + sacks + hits + interceptions + 0, data=rel_pffdatacb)
summary(cbhurries)
#S Model
shurries <- lm(hurries ~ tackles + assists + pff_tfl_proxy + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdatas)
summary(shurries)
#Refine S Hurry Model
shurries <- lm(hurries ~ pff_tfl_proxy + sacks + hits + batted_plus_breakups + 0, data=rel_pffdatas)
summary(shurries)

#Look at Safety plays in nflfastR
safety <- alldef %>% 
  filter(safety == 1)
write.csv(safety, "C:/Users/filim/Documents/R/nflfastR/safety.csv", row.names = FALSE)

#Analyze fumble plays (apportion credit between FF and FR)
idpffs <- alldef %>% 
  filter(idp_forced_fumbles == 1)
mean(idpffs$epa)
write.csv(idpffs, "C:/Users/filim/Documents/R/nflfastR/idpffs.csv", row.names = FALSE)

alldef %>% 
  group_by(idp_forced_fumbles, idp_opp_fumble_recoveries) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()

#Section out FF credit with QBH, sack & sack yards credited
ffreg <- lm(minus_epa ~ 
                   tackles_for_loss + 
                   qb_hits + 
                   sack + 
                   sack_yards + 
                   safety + 
                   idp_forced_fumbles, 
                 data=alldef)
summary(ffreg)
#Check whether Tackles are also recorded on FFs
alldef %>% 
  group_by(idp_forced_fumbles, idp_solos, idp_assists) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()

#Check EPA of recovered fumbles by defense
idpfrs <- alldef %>% 
  filter(idp_opp_fumble_recoveries == 1)
mean(idpfrs$epa)
mean(idpfrs$yards_gained)

#Look at unforced fumble recovery rate to land on FR value
unforcedfumbles <-  alldef %>% 
  filter(fumbles_off == 1 &
           idp_forced_fumbles == 0)
unforcedfumbles %>% 
  group_by(idp_opp_fumble_recoveries) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()

#Now pull out recovery yards and TD coefficients
frreg <- lm(minus_epa ~ 
              idp_opp_fumble_recovery_yards +
              idp_opp_fumble_recovery_tds +
              idp_fumbles,
            data=idpfrs)
summary(frreg)

#Repeat the process of filter and regression for INTs
interceptions <- alldef %>% 
  filter(interception == 1)
mean(interceptions$epa)
write.csv(interceptions, "C:/Users/filim/Documents/R/nflfastR/interceptions.csv", row.names = FALSE)

#see how return yards work when there is both an INT and FR on a play
intandfr <- alldef %>% 
  filter(interception == 1 &
           idp_opp_fumble_recoveries >= 1)

#No doubles means we suspect return yards has integrity.
#Now we check all instances of return yards just in case
idpreturns <- alldef %>% 
  filter(return_yards != 0 |
           idp_opp_fumble_recovery_yards != 0)
idpreturncheck <- idpreturns %>% 
  group_by(interception, return_yards, idp_opp_fumble_recoveries, idp_opp_fumble_recovery_yards) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()
view(idpreturncheck)

#Run INT regression and see where INTs happen on field
intreg <- lm(minus_epa ~ 
              int_return_yards +
              int_return_td +
               idp_fumbles,
            data=interceptions)
summary(intreg)
mean(interceptions$air_yards)


#Analyze PDs to see what else happens on these plays
passesdefensed <- alldef %>% 
  filter(passes_defensed >= 1)
pdtable <- alldef %>% 
  group_by(passes_defensed, interception, incomplete_pass, int_player_pd) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()
view(pdtable)
write.csv(passesdefensed, "C:/Users/filim/Documents/R/nflfastR/passesdefensed.csv", row.names = FALSE)

#Look more closely at IDP fumbles
idpfumbles <- alldef %>% 
  filter(idp_fumbles >= 1)
idpfumbles %>% 
  group_by(interception, idp_opp_fumble_recoveries) %>% 
  summarize(n = n(), epa=mean(epa)) %>% ungroup()
write.csv(idpfumbles, "C:/Users/filim/Documents/R/nflfastR/idpfumbles.csv", row.names = FALSE)

#Work on defining value of Coverage
#Start with average EPA on "Good Coverage" Play
alldef %>% group_by(pass_attempt, neg_epa) %>% summarize(plays = n()) %>% ungroup()
noidpstatpass %>% group_by(neg_epa) %>% summarize(plays = n(), mean_epa = mean(epa)) %>% ungroup()
idpstatnegepa %>% group_by(pass_attempt) %>% summarize(plays = n(), mean_epa = mean(epa)) %>% ungroup()

#First, check ratio of pass rushers to coverage players
pass_rushers_per_play <- 11*sum(pff_idp_recent$snap_counts_pass_rush)/(sum(pff_idp_recent$snap_counts_pass_rush) + sum(pff_idp_recent$snap_counts_coverage))
cov_players_per_play <- 11*sum(pff_idp_recent$snap_counts_coverage)/(sum(pff_idp_recent$snap_counts_pass_rush) + sum(pff_idp_recent$snap_counts_coverage))

#Determine FAL points per good coverage play
pass_count <- 46343+57009
negepa_noidpstat_count <- 19671
negepa_idpstat_count <- 37338

falpts_per_cover <- -8*(cov_players_per_play/11) *
  (negepa_noidpstat_count/pass_count*-0.786 +
     negepa_idpstat_count/pass_count*(2/3)*-1.12)

#Next, establish fantasy point baselines per cov snap by position
#Check FAL pts allowed per snap
sum(pffdatadi$fal_allowed_cov)/sum(pffdatadi$snap_counts_coverage)
sum(pffdataed$fal_allowed_cov)/sum(pffdataed$snap_counts_coverage)
sum(pffdatalb$fal_allowed_cov)/sum(pffdatalb$snap_counts_coverage)
sum(pffdatacb$fal_allowed_cov)/sum(pffdatacb$snap_counts_coverage)
sum(pffdatas$fal_allowed_cov)/sum(pffdatas$snap_counts_coverage)

#Check FAL pts allowed per target
sum(pffdatadi$fal_allowed_cov)/sum(pffdatadi$targets)
sum(pffdataed$fal_allowed_cov)/sum(pffdataed$targets)
sum(pffdatalb$fal_allowed_cov)/sum(pffdatalb$targets)
sum(pffdatacb$fal_allowed_cov)/sum(pffdatacb$targets)
sum(pffdatas$fal_allowed_cov)/sum(pffdatas$targets)

#Check Air Yards Allowed per reception to determine depth of coverage
di_airyards_per_rec <- sum(pffdatadi$air_yards_against)/sum(pffdatadi$receptions)
ed_airyards_per_rec <- sum(pffdataed$air_yards_against)/sum(pffdataed$receptions)
lb_airyards_per_rec <- sum(pffdatalb$air_yards_against)/sum(pffdatalb$receptions)
cb_airyards_per_rec <- sum(pffdatacb$air_yards_against)/sum(pffdatacb$receptions)
s_airyards_per_rec <- sum(pffdatas$air_yards_against)/sum(pffdatas$receptions)

#Check Proportion of Coverage Snaps to determine players in coverage per play; should add to 1
di_cov_players_per_play <- cov_players_per_play*sum(pffdatadi$snap_counts_coverage)/sum(pff_idp_recent$snap_counts_coverage)
ed_cov_players_per_play <- cov_players_per_play*sum(pffdataed$snap_counts_coverage)/sum(pff_idp_recent$snap_counts_coverage)
lb_cov_players_per_play <- cov_players_per_play*sum(pffdatalb$snap_counts_coverage)/sum(pff_idp_recent$snap_counts_coverage)
cb_cov_players_per_play <- cov_players_per_play*sum(pffdatacb$snap_counts_coverage)/sum(pff_idp_recent$snap_counts_coverage)
s_cov_players_per_play <- cov_players_per_play*sum(pffdatas$snap_counts_coverage)/sum(pff_idp_recent$snap_counts_coverage)

AirYardsAvailablePerPassAtt <- 
  di_cov_players_per_play*di_airyards_per_rec +
  ed_cov_players_per_play*ed_airyards_per_rec +
  lb_cov_players_per_play*lb_airyards_per_rec +
  cb_cov_players_per_play*cb_airyards_per_rec +
  s_cov_players_per_play*s_airyards_per_rec

#Calculate Untargeted Value by Position
di_untargeted <- falpts_per_cover*(di_airyards_per_rec/AirYardsAvailablePerPassAtt)
ed_untargeted <- falpts_per_cover*(ed_airyards_per_rec/AirYardsAvailablePerPassAtt)
lb_untargeted <- falpts_per_cover*(lb_airyards_per_rec/AirYardsAvailablePerPassAtt)
cb_untargeted <- falpts_per_cover*(cb_airyards_per_rec/AirYardsAvailablePerPassAtt)
s_untargeted <- (5/6)*falpts_per_cover*(s_airyards_per_rec/AirYardsAvailablePerPassAtt)

#Calculate Coverage Points and add to data
pff_idp_recent <- pff_idp_recent %>%
  mutate(untargeted_pts = case_when(
    position == "DI" ~ di_untargeted*(snap_counts_coverage - targets),
    position == "ED" ~ ed_untargeted*(snap_counts_coverage - targets),
    position == "LB" ~ lb_untargeted*(snap_counts_coverage - targets),
    position == "CB" ~ cb_untargeted*(snap_counts_coverage - targets),
    position == "S" ~ s_untargeted*(snap_counts_coverage - targets)
  ))%>%
  mutate(coverage_pts = untargeted_pts - fal_allowed_cov - pass_break_ups - interceptions)%>%
  mutate(covpts_persnap = coverage_pts/snap_counts_coverage)%>%
  mutate(covpts_composite = (coverage_pts + pff_cov_grade_pts) / 2)

write.csv(pff_idp_recent, "C:/Users/filim/Documents/R/PFFElite/MyAnalysis/pffidprecent.csv", row.names = FALSE)

#Now filter out relevant players AGAIN
rel_pffdatadi <-  pff_idp_recent %>%
  filter(position == "DI" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdataed <-  pff_idp_recent %>%
  filter(position == "ED" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdatalb <-  pff_idp_recent %>%
  filter(position == "LB" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdatacb <-  pff_idp_recent %>%
  filter(position == "CB" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdatas <-  pff_idp_recent %>%
  filter(position == "S" & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)
rel_pffdatadb <-  pff_idp_recent %>%
  filter((position == "S" | position == "CB") & 
           snaps_per_game > 35 & 
           snaps_per_game < 85)

#Assess Coverage Points based on data
mean(rel_pffdatadi$coverage_pts)
mean(rel_pffdataed$coverage_pts)
mean(rel_pffdatalb$coverage_pts)
mean(rel_pffdatacb$coverage_pts)
mean(rel_pffdatas$coverage_pts)

#Assess PFF Coverage Points based on data
mean(rel_pffdatadi$pff_cov_grade_pts)
mean(rel_pffdataed$pff_cov_grade_pts)
mean(rel_pffdatalb$pff_cov_grade_pts)
mean(rel_pffdatacb$pff_cov_grade_pts)
mean(rel_pffdatas$pff_cov_grade_pts)

#Assess PFF Coverage Snaps
mean(rel_pffdatadi$snap_counts_coverage)
mean(rel_pffdataed$snap_counts_coverage)
mean(rel_pffdatalb$snap_counts_coverage)
mean(rel_pffdatacb$snap_counts_coverage)
mean(rel_pffdatas$snap_counts_coverage)

#Predict Coverage Points from IDP stats
dicovpoints <- lm(coverage_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdatadi)
summary(dicovpoints)
edcovpoints <- lm(coverage_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdataed)
summary(edcovpoints)
lbcovpoints <- lm(coverage_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdatalb)
summary(lbcovpoints)
cbcovpoints <- lm(coverage_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdatacb)
summary(cbcovpoints)
scovpoints <- lm(coverage_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions, data=rel_pffdatas)
summary(scovpoints)

#Retry with PD proxy and take out INT
cbcovpoints <- lm(coverage_pts ~ tackles + assists + sacks + hits + forced_fumbles + pff_pd_proxy, data=rel_pffdatacb)
summary(cbcovpoints)
scovpoints <- lm(coverage_pts ~ tackles + assists + sacks + hits + forced_fumbles + pff_pd_proxy, data=rel_pffdatas)
summary(scovpoints)

#Compare Coverage Points to PFF Grade x Snaps
cbgradereg <- lm(pff_cov_grade_pts ~ coverage_pts, data=rel_pffdatacb)
summary(cbgradereg)
sgradereg <- lm(pff_cov_grade_pts ~ coverage_pts, data=rel_pffdatas)
summary(sgradereg)

#Look at Coverage Pts vs PFF Grade x Snaps visually for all DBs
DBcoverageptsplot <- rel_pffdatadb %>%
  ggplot(aes(x = coverage_pts, y = pff_cov_grade_pts, color = position)) +
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(alpha = .6) +
  #add smooth lines fitting data
  stat_smooth(geom='line', size = 1.2, color = 'black', alpha=0.5, se=FALSE, method='lm') +
  # coord_cartesian(xlim = c(5,30), ylim = c(5,25)) would make regression all data but also includes all labels
  xlim(-50, 250) +
  ylim(-50, 250) +
  #titles and caption
  labs(x = "FAFL Cov Pts",
       y = "PFF Cov Pts",
       title = "DB Coverage Analysis",
       subtitle = "PFF vs FAFL",
       caption = "Plot: @AnalyticsFF") +
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold", family="serif"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, face = "bold", family="serif"),
  )
DBcoverageptsplot

#Predict PFF Coverage Points from IDP stats
dicovpoints <- lm(pff_cov_grade_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdatadi)
summary(dicovpoints)
edcovpoints <- lm(pff_cov_grade_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdataed)
summary(edcovpoints)
lbcovpoints <- lm(pff_cov_grade_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdatalb)
summary(lbcovpoints)
cbcovpoints <- lm(pff_cov_grade_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdatacb)
summary(cbcovpoints)
scovpoints <- lm(pff_cov_grade_pts ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdatas)
summary(scovpoints)

#See if regressing on composite gives more stable data
dicovpoints <- lm(covpts_composite ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdatadi)
summary(dicovpoints)
edcovpoints <- lm(covpts_composite ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdataed)
summary(edcovpoints)
lbcovpoints <- lm(covpts_composite ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdatalb)
summary(lbcovpoints)
cbcovpoints <- lm(covpts_composite ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdatacb)
summary(cbcovpoints)
scovpoints <- lm(covpts_composite ~ tackles + assists + sacks + hits + forced_fumbles + batted_plus_breakups + interceptions + penalties, data=rel_pffdatas)
summary(scovpoints)
