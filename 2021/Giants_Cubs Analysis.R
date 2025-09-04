##### MLB Analysis: San Francisco Giants vs Chicago Cubs #####
## By: Stephan Teodosescu
## Updated June 2021

##### Load libraries #####
library(tidyverse)
library(ggridges)
library(teamcolors)
library(forcats)
library(gt) #for 538-themed tables
library(extrafont) #for adding in new fonts
library(rvest) #for web scraping
library(ggalt) #for dumbbell plot
library(ggtext) #for sprucing up ggplot graphics using HTML and CSS stylings
library(teamcolors)


##### FB-Ref data for advanced analytics plots #####

# Load the data (Squad Shooting data set for the 2020-21 season)...remember to update
# Link: https://www.baseball-reference.com/teams/SFG/2021-schedule-scores.shtml

giants_games <- read_csv("Giants_Games_6.10.21.csv")
View(giants_games)


# Custom theme (inspired by Owen Phillips at the F5)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

giants_games <- giants_games %>% 
    filter(is.na(`X5`)) # Filtering for home games only

# Make Plot
ggplot(giants_games, aes(x = `Gm#`, y = Attendance)) +
    geom_line(size = 1.2, color = "#FD5A1E", na.rm = TRUE) +
    geom_point(size = 1.5, color = "#FD5A1E", na.rm = TRUE) +
    annotate("text", x = 70, y = 12792, label = "June 5 vs. CHC") +
    labs(x = "Game No.", y = "Attendance",
         title = "Take Me Out to the Ballgame",
         subtitle = glue("Rolling attendance figures for San Francisco Giants home games at Oracle Park, 2021 ."),
         caption = "Data: baseball-reference\nGraphic: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face="bold")) +
    ggsave("Giants Attendance.png")

##### Cubs #####
# https://www.baseball-reference.com/teams/CHC/2021-schedule-scores.shtml

cubs_games <- read_csv("Cubs_Games.csv")
View(cubs_games)

cubs_games <- cubs_games %>% 
    filter(is.na(`X5`)) # Filtering for home games only

# Make Plot
ggplot(cubs_games, aes(x = `Gm#`, y = Attendance)) +
    geom_line(size = 1.2, color = "#0E3386") +
    geom_point(size = 1.5, color = "#0E3386", na.rm = TRUE) +
    annotate("text", x = 75, y = 22056, label = "Full capacity allowance") +
    labs(x = "Game No.", y = "Attendance",
         title = "Take Me Out to the Ballgame",
         subtitle = glue("Rolling attendance figures for Chicago Cubs home games at Wrigley Field, 2021."),
         caption = "Data: baseball-reference\nGraphic: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face="bold")) +
    ggsave("Cubs Attendance.png")


