# Stage 1 - You should submit predicted probabilities for every possible matchup in the past 4 NCAA tournaments (2012-2015).
# Stage 2 - You should submit predicted probabilities for every possible matchup before the 2016 tournament begins.
# Refer to the Timeline page for specific dates. In both stages, the sample submission will tell you which games to predict.

setwd('kaggle/NCAA/')

library(dplyr)
library(readr)
RegularSeasonCompactResults <- read_csv("RegularSeasonCompactResults.csv")
RegularSeasonDetailedResults <- read_csv("RegularSeasonDetailedResults.csv")

Seasons <- read_csv("Seasons.csv") # W和X对阵，Y和Z对阵 Dayzero+daynum是比赛日期
# 赢得球队，得分；输的球队，得分；主场or客场；打几个加时
TourneyCompactResults <- read_csv("TourneyCompactResults.csv") 
SampleSubmission <- read_csv("SampleSubmission.csv")                                 
Teams <- read_csv("Teams.csv") # 球队的名称及所对应的ID
TourneyDetailedResults <- read_csv("TourneyDetailedResults.csv")
TourneySeeds <- read_csv("TourneySeeds.csv")
# 根据种子排名来决定交战双方，slot是唯一标识一场比赛
TourneySlots <- read_csv("TourneySlots.csv") 

TourneySlots <- TourneySlots %>% 
  left_join(TourneySeeds, by=c("Season", "Strongseed"="Seed")) %>% 
  rename(Strongteam=Team) %>% 
  left_join(TourneySeeds, by=c("Season", "Weakseed"="Seed")) %>% 
  rename(Weakteam=Team)

