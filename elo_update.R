#### MLB ELO Ratings ####

library(baseballr)
library(tidyverse)
library(gt)
library(janitor)
library(glue)
library(progress)

# -------------------------------
# READ GAME DATA
# -------------------------------

games <- baseballr::mlb_schedule(season = "2025") %>%
    filter(game_type == 'R') %>%
    select(teams_away_team_name, teams_away_score, teams_home_team_name, teams_home_score) %>%
    rename(away_team = teams_away_team_name,
           away_score = teams_away_score,
           home_team = teams_home_team_name,
           home_score = teams_home_score)

# Separate played games (complete) from unplayed games (missing scores)
played_games <- games %>%
    filter(!is.na(home_score), !is.na(away_score))

unplayed_games <- games %>%
    filter(is.na(home_score) | is.na(away_score))

# -------------------------------
# PARAMETERS
# -------------------------------

initial_elo <- 1500   # Starting Elo rating for all teams
k_factor <- 20        # Elo update sensitivity
home_field_adv <- 35  # Home field Elo boost

# -------------------------------
# FUNCTIONS
# -------------------------------

# Expected win probability
expected_result <- function(elo_team, elo_opponent) {
    1 / (1 + 10 ^ ((elo_opponent - elo_team) / 400))
}

# Elo update
update_elo <- function(elo, expected, actual, k = k_factor) {
    elo + k * (actual - expected)
}

# -------------------------------
# INITIALIZE TEAM ELO RATINGS
# -------------------------------

teams <- unique(c(games$home_team, games$away_team))

elo_ratings <- tibble(
    team = teams,
    elo = initial_elo
)

# -------------------------------
# INITIALIZE ELO HISTORY TRACKING
# -------------------------------

elo_history <- tibble(
    game_number = integer(),
    home_team = character(),
    away_team = character(),
    home_elo_before = double(),
    away_elo_before = double(),
    home_elo_after = double(),
    away_elo_after = double(),
    home_score = double(),
    away_score = double()
)

# -------------------------------
# PROCESS GAMES & UPDATE ELO
# -------------------------------

for (i in seq_len(nrow(played_games))) {
    
    game <- played_games[i, ]
    
    # Current Elo ratings
    home_elo <- elo_ratings %>% filter(team == game$home_team) %>% pull(elo) %>% .[1]
    away_elo <- elo_ratings %>% filter(team == game$away_team) %>% pull(elo) %>% .[1]
    
    if (is.na(home_elo) | is.na(away_elo)) {
        warning(glue("Missing Elo for one of the teams in game {i}"))
        next
    }
    
    # Apply home field advantage
    home_elo_adj <- home_elo + home_field_adv
    
    # Expected results
    home_exp <- expected_result(home_elo_adj, away_elo)
    away_exp <- 1 - home_exp
    
    # Actual results
    if (game$home_score > game$away_score) {
        home_act <- 1
        away_act <- 0
    } else {
        home_act <- 0
        away_act <- 1
    }
    
    # Update Elo ratings
    home_elo_new <- update_elo(home_elo, home_exp, home_act)
    away_elo_new <- update_elo(away_elo, away_exp, away_act)
    
    # Save updated Elo ratings
    elo_ratings <- elo_ratings %>%
        mutate(
            elo = case_when(
                team == game$home_team ~ home_elo_new,
                team == game$away_team ~ away_elo_new,
                TRUE ~ elo
            )
        )
    
    # Append to elo_history
    elo_history <- elo_history %>%
        add_row(
            game_number = i,
            home_team = game$home_team,
            away_team = game$away_team,
            home_elo_before = home_elo,
            away_elo_before = away_elo,
            home_elo_after = home_elo_new,
            away_elo_after = away_elo_new,
            home_score = game$home_score,
            away_score = game$away_score
        )
}

# -------------------------------
# FINAL ELO TABLE
# -------------------------------

elo_ratings <- elo_ratings %>%
    arrange(desc(elo))

#print(elo_ratings)

saveRDS(elo_ratings, "data/latest_elo.rds")

