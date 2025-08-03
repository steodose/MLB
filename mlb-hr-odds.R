#### 2025 Home Run Odds Chase ####
library(dplyr)
library(jsonlite) # for getting data 

# This repository automatically fetches daily MLB HR champ odds from RotoWire and archives the data 
# as CSV files via GitHub Action.

# get today's date
today <- Sys.Date() 

# ensure data output folder exists
if (!dir.exists("data_hr")) dir.create("data_hr")

# url for getting mvp odds from rotowire
url <- "https://www.rotowire.com/betting/mlb/tables/player-futures.php?future=Home+Run+Leader"

# get data
result <- fromJSON(txt=url) 

# add today's date as a column
df <- result %>% 
    mutate(date = today)

df_clean <- df %>% 
    select(future:logo,mgm_odds, mgm_winPct) %>% 
    left_join(mlb_teamcolors %>% 
              select(team_abbr, division, primary, team_logo_espn), by = c("team" = "team_abbr"))


# write dataframe to .csv in a folder called "data/"
write.csv(df_clean, paste0("data_hr/hr_odds_", gsub("-", "_", today), ".csv"), row.names = F)