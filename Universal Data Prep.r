######--- This file is intended to prep all countries data ---######

# Load packages:
require(plyr)
require(data.table)

# which project folder we want to work in
setwd ("C:/Users/coloughlin/OneDrive/SONY_16M1/Football Predictions/Europe/Input Data")

# Read in the data files and merge to create one European df
Eng_df <- read.csv("England Prepped Input.csv", header = T)
Fra_df <- read.csv("France Prepped Input.csv", header = T)
Ger_df <- read.csv("Germany Prepped Input.csv", header = T)
Spa_df <- read.csv("Spain Prepped Input.csv", header = T)

df <- rbindlist(list(Eng_df,Fra_df,Ger_df,Spa_df), use.names=T)

# so what we need to do is create a unique list of the teams involved in the
# latest season
Teams <- df$HomeTeam
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)
TeamData <- data.frame()

# Then what we do is create the dataset so that instead of having the raw data
# and home and away teams we create a data set where
# we have each team and their associated opposition for the game week, easier to
# see correlations that make sense that way
# below we split it by home and away and then match up by team

for(i in 1:nrow(Teams)){
HData <- df[df$HomeTeam == Teams[i,1],]
# Create the variables that we need - first for all the home matches
HData$Team_Favourite = HData$Home_Favourite
HData$Opposition = HData$AwayTeam
HData$Team_Form = HData$Home_Form
HData$Opposition_Form = HData$Away_form
HData$Team_Shots_on_Target_Form = HData$Home_Shots_on_Target_Form
HData$Opposition_Shots_on_Target_Form = HData$Away_Shots_on_Target_Form
HData$Team_Shots_Conceded_Form = HData$Home_Shots_Conceded_Form
HData$Opposition_Shots_Conceded_Form = HData$Away_Shots_Conceded_Form
HData$Team_Goals_Scored_Form = HData$Home_Goals_Scored_Form
HData$Opposition_Goals_Scored_Form = HData$Away_Goals_Scored_Form
HData$Team_Goals_Conceded_Form = HData$Home_Goals_Conceded_Form
HData$Opposition_Goals_Conceded_Form = HData$Away_Goals_Conceded_Form
HData$Team_Corners_Form = HData$Home_Corners_Form
HData$Team_Fouls_Form = HData$Home_Team_Fouls_Form
HData$Team_Yellow_Cards = HData$Home_Yellow_Cards
HData$Team_Red_Cards = HData$Home_Red_Cards
HData$Opposition_Corners_Form = HData$Away_Corners_Form
HData$Opposition_Fouls_Form = HData$Away_Team_Fouls_Form
HData$Opposition_Yellow_Cards = HData$Away_Yellow_Cards
HData$Opposition_Red_Cards = HData$Away_Red_Cards
HData$Opposition_Goals_Conceded_Form = HData$Away_Goals_Conceded_Form
HData$Relative_Goals_Form = ((HData$Team_Goals_Scored_Form -
                            HData$Team_Goals_Conceded_Form) -
                                (HData$Opposition_Goals_Scored_Form -
                                    HData$Opposition_Goals_Conceded_Form))
HData$Team_Odds = HData$B365H
HData$Draw_Odds = HData$B365D
HData$Opposition_Odds = HData$B365A
HData$Team_AH_Odds = HData$Ave_AH_Home_Odds
HData$Team_Handicap = HData$Asian_Handicap
HData$Opposition_AH_Odds = HData$Ave_AH_Away_Odds
HData$Team_Tier = HData$Home_Tier
HData$Opposition_Tier = HData$Away_Tier
HData$Team_Goal_Diff = HData$Full_Time_Home_Goals - HData$Full_Time_Away_Goals
HData$Home_Away = rep("Home",nrow(HData))
HData$Poisson_Form_Team <- HData$Poisson_Home_Win
HData$Poisson_Form_Draw <- HData$Poisson_Draw
HData$Poisson_Form_Opposition <- HData$Poisson_Away_Win
HData$Poisson_Result <- ifelse(HData$Poisson_Form_Team >
                                HData$Poisson_Form_Opposition,1,
                                ifelse(HData$Poisson_Form_Team <
                                HData$Poisson_Form_Opposition,-1,0))
HData$Regress_Mean_Team <- HData$Regress_Home
HData$Regress_Mean_Draw <- HData$Regress_Draw
HData$Regress_Mean_Opposition <- HData$Regress_Away
HData$Regress_Result <- ifelse(HData$Regress_Mean_Team >
                                HData$Regress_Mean_Opposition,1,
                                ifelse(HData$Regress_Mean_Team <
                                HData$Regress_Mean_Opposition,-1,0))

#-now create the data for the away matches
AData <- df[df$AwayTeam == Teams[i,1],]
#-Create the variables that we need - for all the Away matches
AData$Team_Favourite = AData$Away_Favourite
AData$Opposition = AData$HomeTeam
AData$Team_Form = AData$Away_form
AData$Opposition_Form = AData$Home_Form
AData$Team_Shots_on_Target_Form = AData$Away_Shots_on_Target_Form
AData$Opposition_Shots_on_Target_Form = AData$Home_Shots_on_Target_Form
AData$Team_Shots_Conceded_Form = AData$Away_Shots_Conceded_Form
AData$Opposition_Shots_Conceded_Form = AData$Home_Shots_Conceded_Form
AData$Team_Goals_Scored_Form = AData$Away_Goals_Scored_Form
AData$Opposition_Goals_Scored_Form = AData$Home_Goals_Scored_Form
AData$Team_Goals_Conceded_Form = AData$Away_Goals_Conceded_Form
AData$Opposition_Goals_Conceded_Form = AData$Home_Goals_Conceded_Form
AData$Team_Corners_Form = AData$Away_Corners_Form
AData$Team_Fouls_Form = AData$Away_Team_Fouls_Form
AData$Team_Yellow_Cards = AData$Away_Yellow_Cards
AData$Team_Red_Cards = AData$Away_Red_Cards
AData$Opposition_Corners_Form = AData$Home_Corners_Form
AData$Opposition_Fouls_Form = AData$Home_Team_Fouls_Form
AData$Opposition_Yellow_Cards = AData$Home_Yellow_Cards
AData$Opposition_Red_Cards = AData$Home_Red_Cards
AData$Opposition_Goals_Conceded_Form = AData$Home_Goals_Conceded_Form
AData$Relative_Goals_Form = ((AData$Team_Goals_Scored_Form -
                                AData$Team_Goals_Conceded_Form) -
                                (AData$Opposition_Goals_Scored_Form -
                                    AData$Opposition_Goals_Conceded_Form))
AData$Team_Odds = AData$B365A
AData$Draw_Odds = AData$B365D
AData$Opposition_Odds = AData$B365H
AData$Team_AH_Odds = AData$Ave_AH_Away_Odds
AData$Team_Handicap = AData$Asian_Handicap*-1
AData$Opposition_AH_Odds = AData$Ave_AH_Home_Odds
AData$Team_Tier = AData$Away_Tier
AData$Opposition_Tier = AData$Home_Tier
AData$Team_Goal_Diff = AData$Full_Time_Away_Goals - AData$Full_Time_Home_Goals
AData$Home_Away = rep("Away",nrow(AData))
AData$Poisson_Form_Team <- AData$Poisson_Away_Win
AData$Poisson_Form_Draw <- AData$Poisson_Draw
AData$Poisson_Form_Opposition <- AData$Poisson_Home_Win
AData$Poisson_Result <- ifelse(AData$Poisson_Form_Team >
                                AData$Poisson_Form_Opposition,1,
                                ifelse(AData$Poisson_Form_Team <
                                AData$Poisson_Form_Opposition,-1,0))
AData$Regress_Mean_Team <- HData$Regress_Away
AData$Regress_Mean_Draw <- HData$Regress_Draw
AData$Regress_Mean_Opposition <- AData$Regress_Away
AData$Regress_Result <- ifelse(AData$Regress_Mean_Team >
                                AData$Regress_Mean_Opposition,1,
                                ifelse(AData$Regress_Mean_Team <
                                AData$Regress_Mean_Opposition,-1,0))

# brill now we're cooking
# so now that we have those two datasets let's append them and add them to
# our major dataframe
ComboData <- HData
ComboData <- rbind(ComboData,AData)
Team <- rep(Teams[i,1],nrow(ComboData))
ComboData <- cbind(ComboData,Team)
TeamData <- rbindlist(list(TeamData,ComboData))
TeamData <- as.data.frame(TeamData)
}


# kl
# nope it'll never catch on I mean "cool"

write.csv(TeamData, "C:/Users/coloughlin/OneDrive/SONY_16M1/Football Predictions/Europe/Output Data/Europe Prepped Output"), row.names=F)
}
