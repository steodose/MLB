#2017 MLB Game Log Analysis

#Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(readr)
library(lubridate)
library(stringr)
library(mosaic)
library(viridis)
library(RColorBrewer)
library(grid)
library(gridExtra)

#Set working directory

#Load 2017 game data from Retrosheet.org
GL2017 <- read.table("GL2017.TXT", header = FALSE, sep = ",", 
                     fill = TRUE)

#Variable 23 is visiting team hits; variable 51 is home team hits

#Plot frequency of away team hits
ggplot(GL2017, aes(x = `V23`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Away Team Hits", 
       title = "MLB Hits in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

#Plot frequency of home team hits
ggplot(GL2017, aes(x = `V51`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Home Team Hits", 
       title = "MLB Hits in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

#Refine analysis for San Francisco Giants games
GL2017_SF <- GL2017 %>%                    #All Giants games
  filter(V4 == "SFN" | V7 == "SFN")

GL2017_SF_Away <- GL2017_SF %>%            #Away games
  filter(V4 == "SFN")

GL2017_SF_Home <- GL2017_SF %>%            #Home games
  filter(V7 == "SFN")

#Giants as the away team
SFG_Away <- ggplot(GL2017_SF_Away, aes(x = `V23`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "SF Giants (Away) Hits", 
       title = "San Francisco Giants Hits in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

#Giants as the home team
SFG_Home <- ggplot(GL2017_SF_Home, aes(x = `V51`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "SF Giants (Home) Hits", 
       title = "San Francisco Giants Hits in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))


#Refine analysis for Pittsburgh Pirates games
GL2017_PIT <- GL2017 %>%
  filter(V4 == "PIT" | V7 == "PIT")

GL2017_PIT_Away <- GL2017_PIT %>%            #Away games
  filter(V4 == "PIT")

GL2017_PIT_Home <- GL2017_PIT %>%            #Home games
  filter(V7 == "PIT")

#Pirates as the away team
PIT_Away <- ggplot(GL2017_PIT_Away, aes(x = `V23`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Pittsburgh Pirates (Away) Hits", 
       title = "Pittsburgh Pirates Hits in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

#Pirates as the home team
PIT_Home <- ggplot(GL2017_PIT_Home, aes(x = `V51`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Pittsburgh Pirates (Home) Hits", 
       title = "Pittsburgh Pirates Hits in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

#Create a grid of the plots
grid.arrange(SFG_Home, SFG_Away, PIT_Home, PIT_Away, ncol = 2)

#Refine analysis for Chicago Cubs games
GL2017_CHC <- GL2017 %>%
  filter(V4 == "CHN" | V7 == "CHN")

GL2017_CHC_Away <- GL2017_CHC %>%            #Away games
  filter(V4 == "CHN")

GL2017_CHC_Home <- GL2017_CHC %>%            #Home games
  filter(V7 == "CHN")

#Cubs as the away team
CHC_Away <- ggplot(GL2017_CHC_Away, aes(x = `V23`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Chicago Cubs (Away) Hits", 
       title = "Chicago Cubs Hits in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

#Cubs as the home team
CHC_Home <- ggplot(GL2017_CHC_Home, aes(x = `V51`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Chicago Cubs (Home) Hits", 
       title = "Chicago Cubs Hits in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

#Create a grid of the plots
grid.arrange(CHC_Home, CHC_Away, ncol = 2)

#-------------------------------------------------------------

#Select only variables I'm interested in to make a smaller data frame
MLB_2017 <- GL2017 %>%
  select(V1, V3, V4, V7, V10, V11, V17, V18, V20, V21, V23, V51)

#Name the column headers
colnames(MLB_2017) <- c("Date", "Day of Week", "Visitor", "Home Team",
                          "Visitor Score", "Home Score", "Park ID", "Attendance",
                          "Visitor Line", "Home Line", "Visitor Hits",
                          "Home Hits")

MLB_2017 <- as_tibble(MLB_2017) #Convert to a tibble

#Boxplot comparing attendance over the week
ggplot(data = MLB_2017, 
       aes(x = `Day of Week`, y = Attendance)) + 
  geom_boxplot() + 
  xlab("Day of Week") + ylab("Attendance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("MLB Attendance 2017") +
  labs(caption = "Data Source: Retrosheet.org") +
  guides(fill=FALSE)

#Cubs attendance
MLB_2017$Date <- ymd(MLB_2017$Date)

MLB_2017 %>%
  filter(`Home Team` == "CHN") %>%
ggplot(aes(x=Date, y=Attendance)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Chicago Cubs Attendance 2017") +
  labs(caption = "Data Source: Retrosheet.org")

#MLB Scores
ggplot(MLB_2017, aes(x = `Home Score`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Runs Scored By Home Team", 
       title = "MLB Runs Scored in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(MLB_2017, aes(x = `Visitor Score`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Runs Scored By Visiting Team", 
       title = "MLB Runs Scored in 2017",
       caption = "Data Source: Retrosheet.org") +
  theme(plot.title = element_text(hjust = 0.5))

#Inning by inning analysis
MLB_Innings <- MLB_2017 %>%
  select(Date, Visitor, `Home Team`, `Visitor Score`, 
         `Home Score`, `Visitor Line`, `Home Line`)



