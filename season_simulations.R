#### MLB ELO Season Simulations ####

library(tidyverse)
library(glue)
library(progress)

# -------------------------------
# PARAMETERS
# -------------------------------

initial_elo <- 1500
k_factor <- 20
home_field_adv <- 35
num_sims <- 1000
avg_runs_per_team <- 4.5  # Base expected runs per team per game

# -------------------------------
# FUNCTIONS
# -------------------------------

expected_result <- function(elo_team, elo_opponent) {
    1 / (1 + 10 ^ ((elo_opponent - elo_team) / 400))
}

update_elo <- function(elo, expected, actual, k = k_factor) {
    elo + k * (actual - expected)
}

# -------------------------------
# SIMULATION SETUP
# -------------------------------

# Copy current Elo ratings
temp_elo <- elo_ratings
teams <- temp_elo$team

div_lookup <- tibble(
    team = teams,
    division = case_when(
        str_detect(team, "Dodgers|Giants|Padres|D-backs|Rockies") ~ "NL West",
        str_detect(team, "Cubs|Cardinals|Brewers|Reds|Pirates") ~ "NL Central",
        str_detect(team, "Braves|Phillies|Mets|Marlins|Nationals") ~ "NL East",
        str_detect(team, "Yankees|Red Sox|Blue Jays|Rays|Orioles") ~ "AL East",
        str_detect(team, "Tigers|Guardians|White Sox|Royals|Twins") ~ "AL Central",
        str_detect(team, "Astros|Mariners|Rangers|Angels|Athletics") ~ "AL West",
        TRUE ~ NA_character_
    )
)

sim_results <- tibble()
pb <- progress_bar$new(total = num_sims)

# -------------------------------
# MAIN SIMULATION LOOP
# -------------------------------

for (sim in 1:num_sims) {
    
    sim_elo <- temp_elo
    sim_stats <- tibble(team = teams, wins = 0, rs = 0, ra = 0)
    
    for (i in seq_len(nrow(unplayed_games))) {
        
        game <- unplayed_games[i, ]
        home_elo <- sim_elo %>% filter(team == game$home_team) %>% pull(elo) %>% .[1]
        away_elo <- sim_elo %>% filter(team == game$away_team) %>% pull(elo) %>% .[1]
        
        home_elo_adj <- home_elo + home_field_adv
        home_exp <- expected_result(home_elo_adj, away_elo)
        
        # Simulate winner
        home_win <- rbinom(1, 1, home_exp)
        
        # Simulate runs scored (Poisson with slight Elo adjustment)
        home_runs <- rpois(1, avg_runs_per_team + (home_elo - away_elo) / 1000)
        away_runs <- rpois(1, avg_runs_per_team + (away_elo - home_elo) / 1000)
        
        # Ensure winner's runs > loser's
        if (home_win == 1 && home_runs <= away_runs) home_runs <- away_runs + sample(1:3, 1)
        if (home_win == 0 && away_runs <= home_runs) away_runs <- home_runs + sample(1:3, 1)
        
        # Update stats
        sim_stats <- sim_stats %>%
            mutate(
                wins = case_when(
                    team == game$home_team & home_win == 1 ~ wins + 1,
                    team == game$away_team & home_win == 0 ~ wins + 1,
                    TRUE ~ wins
                ),
                rs = case_when(
                    team == game$home_team ~ rs + home_runs,
                    team == game$away_team ~ rs + away_runs,
                    TRUE ~ rs
                ),
                ra = case_when(
                    team == game$home_team ~ ra + away_runs,
                    team == game$away_team ~ ra + home_runs,
                    TRUE ~ ra
                )
            )
        
        # Update Elo
        home_act <- home_win
        away_act <- 1 - home_win
        home_elo_new <- update_elo(home_elo, home_exp, home_act)
        away_elo_new <- update_elo(away_elo, 1 - home_exp, away_act)
        
        sim_elo <- sim_elo %>%
            mutate(
                elo = case_when(
                    team == game$home_team ~ home_elo_new,
                    team == game$away_team ~ away_elo_new,
                    TRUE ~ elo
                )
            )
    }
    
    sim_stats <- sim_stats %>%
        left_join(div_lookup, by = "team") %>%
        group_by(division) %>%
        mutate(div_winner = if_else(wins == max(wins), 1, 0)) %>%
        ungroup()
    
    sim_results <- bind_rows(sim_results, sim_stats %>% mutate(sim = sim))
    pb$tick()
}

# -------------------------------
# SUMMARIZE RESULTS
# -------------------------------

summary_df <- sim_results %>%
    group_by(team) %>%
    summarize(
        avg_wins = mean(wins),
        avg_rs = mean(rs),
        avg_ra = mean(ra),
        avg_rd = avg_rs - avg_ra,
        div_pct = mean(div_winner) * 100
    ) %>%
    left_join(div_lookup, by = "team") %>%
    arrange(desc(avg_wins))

print(summary_df)
