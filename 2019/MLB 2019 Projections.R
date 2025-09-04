###### 2019 MLB Projections ######
##### March 27, 2019 ###########
##### Stephan Teodosescu #########

library(tidyverse)
library(RCurl)
library(teamcolors)
library(ggrepel)

## Set working directory

## Load data from 538: every MLB game in history (https://github.com/fivethirtyeight/data/tree/master/mlb-elo)
mlb <- read_csv("mlb_elo.csv")

mlb_2019 <- mlb %>%
  filter(season == 2019) %>%
  select(1:2,5:6,25:26) %>%
  rename(home = team1, away = team2, home_score = score1, 
         away_score = score2) 

##### Determine pre-season ratings #####
mlb_2018 <- mlb %>%
  filter(season == 2018, date < "2018-10-01") %>% #filter for regular season
  select(1:2,5:6,25:26)

mlb_2018$score1 <- as.numeric(mlb_2018$score1) #convert character to numeric (home)
mlb_2018$score2 <- as.numeric(mlb_2018$score2) #convert character to numeric (away)

mlb_2018 <- mlb_2018 %>%
  mutate(Run_Difference_home = score1 - score2,
         Run_Difference_away = score2 - score1)

## Determine run differential for every team in 2018. This will be used to 
## determine pre-season 2019 ratings for every club.

# Home teams
team_ratings_home <- mlb_2018 %>%
  group_by(team1) %>%
  summarise(runs_scored = sum(score1),
            Run_Differential_home = sum(Run_Difference_home))

# Away teams 
team_ratings_away <- mlb_2018 %>%
  group_by(team2) %>%
  summarise(runs_scored = sum(score2), 
            Run_Differential_away = sum(Run_Difference_away))

# Combine into one ratings matrix
team_ratings <- left_join(team_ratings_home, team_ratings_away,
                          by = c("team1" = "team2"))

## Clean up team_ratings matrix
team_ratings <- team_ratings %>%
  mutate(runs_scored = runs_scored.x + runs_scored.y,
         total_run_differential = Run_Differential_home + Run_Differential_away) %>%
  ungroup() %>%
  rename(team = team1, runs_home = runs_scored.x, runs_away = runs_scored.y, 
         run_differential_home = Run_Differential_home, run_differential_away = 
           Run_Differential_away)

## Determine ratings by regressing run differentials to the mean
team_ratings <- team_ratings %>%
  mutate(Rating = total_run_differential/2)

##### Simulate MLB season 10,000 times #####

