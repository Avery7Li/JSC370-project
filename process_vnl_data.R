library(data.table)
library(dplyr)
library(leaflet)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(mgcv)
library(kableExtra)
library("GGally")
library(AICcmodavg)
library(stringr)
library(plotly)

library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(xgboost)
library(caret)

# Helper functions
# Slice from end of the string
slice_back <- function(s, n) {
  sub = substr(s, nchar(s)-n+1, nchar(s))
  return(sub)
}
# Mode
fmode <- function(x) unique(x)[which.max(table(x))]

# Position order constant
position_levels <- c("Opposite Spiker", "Outside Hitter", "Middle Blocker",
                     "Setter", "Libero")

# Read in the player and team data
scorers <- data.table::fread("data/best-scorers.csv")
spikers <- data.table::fread("data/best-spikers.csv")
diggers <- data.table::fread("data/best-diggers.csv")
setters <- data.table::fread("data/best-setters.csv")
receivers <- data.table::fread("data/best-receivers.csv")
blockers <- data.table::fread("data/best-blockers.csv")

bio <- data.table::fread("data/player_bio.csv")
team_rank <- data.table::fread("data/team_rank.csv")

# ***** bio ***** 
# Rename ambiguous columns before merging
setnames(bio, 'spike', 'spike_height')
setnames(bio, 'block', 'block_height')
setnames(bio, 'total', 'total_selections')

# Deal with outliers
bio[spike_height < 240]
nrow(bio[spike_height >= block_height]) / nrow(bio)
bio[, spike_height := fifelse(spike_height < 150, block_height, spike_height)]

# Only one player has a universal position, replace with the most occurred position
bio[position == 'Universal']
bio[, position := fifelse(position == 'Universal', fmode(position), position)]

# Make position name consistent
bio[, position := fifelse(position == 'Middle blocker', 
                          'Middle Blocker', 
                          fifelse(position == 'Opposite spiker',
                                  'Opposite Spiker', position))]

## Create numerical variables - bmi, world_game, age
bio[, bmi := round(weight / (height/100)**2, 1)]
bio[, world_selection := world_championships + olympic_games]
# Take the last four char from birth date and convert to int as birth year
bio[, birth_year := strtoi(slice_back(birthdate, 4))]
# Calculate age
bio[, age:= 2019 - birth_year]
# Create a indicator for captain: name has '\nc' at the end
bio[, is_captain := fifelse(slice_back(name, 2) == '\nc', 1, 0)]
# Remove char from end of name
bio[, name := gsub("\nc", "", name)]
# Keep only relevant columns

## ***** scorers *****
# Rename columns
setnames(scorers, 'total', 'total_score')
setnames(scorers, 'rank', 'score_rank')
# Drop column that's not interesting
scorers <- scorers[, !"shirtnumber"]

# Merge player scores with bio
# look for association between players' individual scores and their ability
players <- merge(x = bio[, .(name, team, position, age, height, weight, bmi,
                             spike_height, block_height,
                             total_selections, is_captain)], 
                 y = scorers, 
                 all.x = T, all.y = F, 
                 by=c("name", 'team'))

## ***** players *****
# Rename
setnames(diggers, 'average_per_set', 'digs_per_set')
setnames(setters, 'average_per_set', 'sets_per_set')
setnames(spikers, 'success_%', 'attack_success_rate')
# Merge with player dataset
players <- merge(x = players, 
                 y = diggers[, .(name, digs_per_set)],
                 all.x = T, all.y = F, 
                 by="name")
players <- merge(x = players, 
                 y = setters[, .(name, sets_per_set)], 
                 all.x = T, all.y = F, 
                 by="name")
players <- merge(x = players, 
                 y = spikers[, .(name, attack_success_rate)], 
                 all.x = T, all.y = F, 
                 by="name")
# Check for NAs
players[is.na(digs_per_set) & is.na(sets_per_set) & 
          is.na(attack_success_rate) & is.na(total_score), .N]
players <- players[!(is.na(digs_per_set) & is.na(sets_per_set) & 
                       is.na(attack_success_rate) & is.na(total_score))]
players[is.na(total_score), .N]   # 43

# Impute missing values
players <- players %>%
  group_by(position) %>%
  mutate(
    total_score = coalesce(total_score, round(mean(total_score, na.rm=TRUE))),
    digs_per_set = coalesce(digs_per_set, round(mean(digs_per_set, na.rm=TRUE))),
    sets_per_set = coalesce(sets_per_set, round(mean(sets_per_set, na.rm=TRUE)))
    #attack_success_rate = coalesce(attack_success_rate,
    #                               round(mean(attack_success_rate, na.rm=TRUE)))
  )
# Get team full name
players <- merge(x=players,
                 y=team_rank[, .(team, team_full)], by='team')
players <-data.table(players)

## ***** Team Rank *****
# Rename team rank
setnames(team_rank, 'rank', 'team_rank')
# Merge team info together with player info
team_players <- merge(x=team_rank[, .(team, team_rank, match_win, 
                                      set_ratio, team_full)], 
                      y=players[, .(name, team, age, position, height, 
                                    total_selections, total_score,
                                    digs_per_set, sets_per_set, 
                                    attack_success_rate)],
                      
                      all.x = T, all.y = T, 
                      by='team')


## ***** Top players *****
player_rank <- rbind(spikers[, .(name, rank, skill = "Attack", value=attack_success_rate)],
                  setters[, .(name, rank, skill = "Set", value=sets_per_set)],
                  diggers[, .(name, rank, skill = "Dig", value=digs_per_set)],
                  #receivers[, .(name, rank, skill = "Receive", value=`efficiency_%`)],
                  blockers[, .(name, rank, skill = "Block", value=average_per_set)])
player_rank <- merge(x=player_rank, 
                  y=bio[, .(team, name, position, is_captain)], 
                  all.x=T, all.y=F, by='name')
player_rank <- merge(x=player_rank, 
                  y=team_rank[, .(team_rank, team_full, team)], 
                  all.x=T, all.y=F, by='team')



##  ***** Round robin *****
matches <- data.table::fread("data/round_robin.csv")
matches[, team_home:=sapply(strsplit(as.character(matches$teams),'-'), "[", 1)]
matches[, team_away:=sapply(strsplit(as.character(matches$teams),'-'), "[", 2)]
matches[, set_home:=sapply(strsplit(as.character(matches$sets),'-'), "[", 1)]
matches[, set_away:=sapply(strsplit(as.character(matches$sets),'-'), "[", 2)]
matches[, point_home:=sapply(strsplit(as.character(matches$pionts),'-'), "[", 1)]
matches[, point_away:=sapply(strsplit(as.character(matches$pionts),'-'), "[", 2)]
# Calculate the number of sets won by each team in a match
matches<-transform(matches, set_home = as.numeric(set_home), 
                   set_away = as.numeric(set_away),
                   point_home = as.numeric(point_home), 
                   point_away = as.numeric(point_away))
matches[, set_home_win:=set_home-set_away]
matches[, set_away_win:=set_away-set_home]
matches[, point_home_win:=point_home-point_away]
matches[, point_away_win:=point_away-point_home]

match_each_team <- rbind(matches[, .(team=team_home, opponent=team_away, 
                                     sets_win=set_home_win, points_win=point_home_win,
                                     sets=sets, date=date)],
                         matches[, .(team=team_away, opponent=team_home, 
                                     sets_win=set_away_win, points_win=point_away_win,
                                     sets=sets, date=date)])
# Order based on team name and date
match_each_team<-match_each_team[order(team, date)]
# Add a column of index for each team
match_each_team$index <- rep(1:15, times=16)

## Teams in order
team_abr_in_order <- team_rank[, team]
team_in_order <- team_rank[, team_full]



##  ***** VNL2021 Per Match Data *****
matches_2021 <- data.table::fread("data/matches2021.csv")
schedule_2021 <- data.table::fread("data/schedule2021.csv")
setnames(matches_2021, "nationality", "team")
setnames(schedule_2021, "awayhome", "scoreaway")

# For attack, block, and serve, use the total efficiency, i.e. (points - error) / total
match_summary <- matches_2021[, 
                              .(attack_eff=round((sum(attack_pt)-sum(attack_err))/sum(attack_tot), 4), 
                                block_eff=round((sum(block_pt)-sum(block_err))/sum(block_tot), 4), 
                                serve_eff=round((sum(serve_pt)-sum(serve_err))/sum(serve_tot), 4)), 
                              by=c("schedule_id", "team")]

## For set, reception, and digger, use the maximum record 
# (verify if the position matches with the skill)
# set
set_per_match <- matches_2021[matches_2021[, .I[set_tot == max(set_tot)], by=c("schedule_id", "team")]$V1, .(team, schedule_id, name, position, set_pt, set_err, set_tot, set_eff)]
setnames(set_per_match, "name", "set_name")
set_per_match$set_eff <- set_per_match$set_eff/100
# reception
reception_per_match <- matches_2021[matches_2021[, .I[reception_tot == max(reception_tot)], by=c("schedule_id", "team")]$V1, .(team, schedule_id, name, position, reception_successful, reception_err, reception_tot, reception_eff)]
setnames(reception_per_match, "name", "reception_name")
reception_per_match$reception_eff <- reception_per_match$reception_eff/100
# dig
dig_per_match <- matches_2021[matches_2021[, .I[dig_tot == max(dig_tot)], by=c("schedule_id", "team")]$V1, .(team, schedule_id, name, position, dig_digs, dig_err, dig_tot, dig_eff)]
setnames(dig_per_match, "name", "dig_name")
not_liberer_dig <- dig_per_match[position != "L"&position != "OH"]
dig_per_match$dig_eff <- dig_per_match$dig_eff/100
# best scorer TODO
## Combine
match_summary <- merge(x=match_summary, 
                       y=unique(set_per_match, 
                                by=c("schedule_id", "team"))[, -"position"],
                       all.x = T, all.y = T, 
                       by=c("schedule_id", "team"))
match_summary <- merge(x=match_summary, 
                       y=unique(reception_per_match, 
                                by=c("schedule_id", "team"))[, -"position"],
                       all.x = T, all.y = T, 
                       by=c("schedule_id", "team"))
match_summary <- merge(x=match_summary, 
                       y=unique(dig_per_match, 
                                by=c("schedule_id", "team"))[, -"position"],
                       all.x = T, all.y = T, 
                       by=c("schedule_id", "team"))

### Add overall match info
schedule_2021[, gender:=sapply(strsplit(as.character(schedule_2021$matchname),' - '), "[", 3)]
# Get opponent team name and scores
match_summary <- merge(x=match_summary, 
                       y=schedule_2021[, .(matchid, teamhome, scorehome, 
                                           teamaway, scoreaway, gender,
                                           points_home)],
                       all.x = T, all.y = F, 
                       by.x=c("schedule_id", "team"), by.y=c("matchid", "teamhome"))
setnames(match_summary, "scorehome", "score")
setnames(match_summary, "teamaway", "opponent")
setnames(match_summary, "scoreaway", "opponent_score")
setnames(match_summary, "points_home", "points")

match_summary <- merge(x=match_summary, 
                       y=schedule_2021[, .(matchid, teamhome, scorehome, 
                                           teamaway, scoreaway, gender, 
                                           points_away)],
                       all.x = T, all.y = F, 
                       by.x=c("schedule_id", "team"), by.y=c("matchid", "teamaway"))
setnames(match_summary, "gender.x", "gender")
match_summary$score <- ifelse(is.na(match_summary$score), 
                              match_summary$scoreaway, match_summary$score)
match_summary$opponent <- ifelse(is.na(match_summary$opponent), 
                                 match_summary$teamhome, match_summary$opponent)
match_summary$opponent_score <- ifelse(is.na(match_summary$opponent_score), 
                                       match_summary$scorehome, match_summary$opponent_score)
match_summary$gender <- ifelse(is.na(match_summary$gender), 
                               match_summary$`gender.y`, match_summary$gender)
match_summary$points <- ifelse(is.na(match_summary$points), 
                               match_summary$points_away, match_summary$points)
# Drop extra columns
match_summary[, c("teamhome", "scorehome", "scoreaway", "gender.y", "points_away"):=NULL]
match_summary[, sets_win:=score-opponent_score]
# Get opponent block efficiency
match_summary <- merge(x=match_summary, 
                       y=match_summary[, .(schedule_id, team, block_eff, points)],
                       all.x = T, all.y = F, 
                       by.x=c("schedule_id", "opponent"), 
                       by.y=c("schedule_id", "team"))
setnames(match_summary, "block_eff.y", "opponent_block_eff")
setnames(match_summary, "block_eff.x", "block_eff")
setnames(match_summary, "points.y", "opponent_points")
setnames(match_summary, "points.x", "points")
match_summary[, point_diff := points - opponent_points]