##2018 MLB Predictions
library(ggplot2)
library(dplyr)
library(knitr)
library(readr)

#load data
baseball <- read_csv("baseball.csv")
View(baseball)

#Load 538 Elo Ratings
mlb_elo <- read_csv("mlb_elo.csv")
View(mlb_elo)

# Subset to only include moneyball years
moneyball <- subset(baseball, Year > 1990)

# Compute Run Difference
moneyball <- moneyball %>%
  mutate(Run_Differential = RS - RA)

ggplot(moneyball, aes(x=Run_Differential, y=W)) +
  geom_point(alpha = 1/2) +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "Run Differential", 
       y = "Wins", 
       title = "MLB Projected Wins since 1990")

#Highlighting the Cubs
cubs <- moneyball %>%
  filter(Team == "CHC")

#Loading library of pro sports team colors
library(teamcolors)
teamcolors1 <- teamcolors %>% 
  filter(league == "mlb") %>% 
  rename(team_id = name) %>% 
  mutate(rand.color = ifelse(primary == "#010101", secondary, primary))

ggplot(moneyball, aes(x=Run_Differential, y=W)) +
  geom_point(color = "grey", alpha = 3/4) +
  geom_point(data = cubs, color = "#002F6C") +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "Run Differential", y = "Wins", 
       title = "Cubs Projected Wins 1991 - 2012") +
  theme(legend.position="none")

# Regression model to predict wins in regular season
WinsReg <- lm(W ~ Run_Differential, data=moneyball)
summary(WinsReg)

#Using Elo dataset
mlb_elo2$score1 <- as.numeric(mlb_elo2$score1) #converting characters to numerics
mlb_elo2$score2 <- as.numeric(mlb_elo2$score2)

mlb_elo2 <- mlb_elo %>%
  filter(season >= 2002 & season < 2017) %>%
  mutate(RD = score1 - score2)

