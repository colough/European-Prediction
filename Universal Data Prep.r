######--- This file is intended to prep all countries data ---######

# Load packages:
require(plyr)
require(data.table)

#-which project folder we want to work in
setwd ("C:/Users/coloughlin/Documents/Temp/Update/Football Predictions/Data/Pre Prepped")
Input_Files <- list.files()

for (p in 3:3){
DATA <- read.csv(Input_Files[p], header = TRUE)

# so what we need to do is create a unique list of the teams involved in the
# latest season
Teams <- DATA[,5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)
TeamData <- data.frame()

# Then what we do is create the dataset so that instead of having the raw data
# and home and away teams we create a data set where
# we have each team and their associated opposition for the game week, easier to
# see correlations that make sense that way
# below we split it by home and away and then match up by team

for(i in 1:nrow(Teams)){
HData <- DATA[DATA$HomeTeam == Teams[i,1],]
#-Create the variables that we need - first for all the home matches
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


#-now create the data for the away matches
AData <- DATA[DATA$AwayTeam == Teams[i,1],]
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


# ok so we have a tasty little treat here where we have to order the teamdata
# dataframe by teams seasons and gameweeks and then
# we have to create a lookup table which calculates our basic strength, join
# that to our original then create a win streak count and subsequent probability
# variable, also for shits and giggles as this is just the data manipulation
# stage I'm going to play with data.table
PreppedData <- data.table(TeamData)
PreppedData <- PreppedData[order(Team,Season,Game_Week_Index)]
#-create the points the team scored
PreppedData$Team_Points <- ifelse(PreppedData$Team_Goal_Diff > 0, 3,
                            ifelse(PreppedData$Team_Goal_Diff < 0, 0, 1))
#-creating the lookup table from the average over the first 6 games
AveStr <- PreppedData[Game_Week_Index <= 6,
                        .(Initial_Strength = ave(Team_Points)),
                                                by =.(Season, Team)]
OppStr <- PreppedData[Game_Week_Index <= 6,
                        .(Opp_Initial_Strength = ave(Team_Points)),
                                                by =.(Season, Opposition)]
#- thar be duplicates ahead, remove 'em
#-remove the key
setkey(AveStr,NULL)
AveStr <- unique(AveStr)
setkey(OppStr,NULL)
OppStr <- unique(OppStr)

PreppedData <- join(PreppedData, AveStr, by = c("Season", "Team"))
PreppedData <- join(PreppedData, OppStr, by = c("Season", "Opposition"))
# kl
# nope it'll never catch on I mean "cool"
# ok now I want to add a winning streak variable
# for this one I'm going to have to cheat just a little bit as we need the
# average streak per season what we can then do is
# look at the average ratio of first 6 games average to season and see if there
# is any consistency there if so we can use the first 6
# as a proxy for the season that we are going to be predicting
# so for this bit of code we set up an empty data table and then set it to the
# filtered PreppedData for each team season combo
# Then we add another column which will count a 1 or 0 if the team has not lost
# or has lost subsequently we create a second column which adds up these 1's and
# 0's into a streak

WinStrC <- data.table()
WinStreakComplete <- data.table()
for (e in 1:nrow(AveStr)){
WinStrC <- PreppedData[Team == AveStr[[e,2]] & Season == AveStr[[e,1]],]
		for (r in 1:nrow(WinStrC)){
		WinStrC$Win_ID <- ifelse(WinStrC$Team_Goal_Diff >= 0, 1, 0)
		if(r == 1){
			WinStrC$Win_Count[r] <- ifelse(WinStrC$Win_ID[r] >0, 1,0)
			}else{
			WinStrC$Win_Count[r] <- ifelse(WinStrC$Win_ID[r] >0,
                                        WinStrC$Win_Count[[r-1]]+1,0)
			}
		}
		if(r == 1){
		WinStreakComplete <- WinStrC
		}else{
			WinStreakComplete <- rbind(WinStreakComplete,WinStrC)
			}

}
ncol(WinStreakComplete)
PreppedData <- WinStreakComplete
rm(WinStreakComplete)
# now figure out what the average streak is for each team across the
# model time period
AveStreak <- PreppedData[,.(Ave_Streak = ave(Win_Count)), by =.(Team)]
AveStreak <- unique(AveStreak)
PreppedData <- join(PreppedData, AveStreak, by = c("Team"))

#-finally ready to add on what the chances are of increasing the non lose streak
for(u in 1:nrow(PreppedData)){
	if(u == 1){
	PreppedData$Streak_Probability[[1]] <- dpois(PreppedData$Win_Count[[1]],
                                                PreppedData$Ave_Streak[[1]])
	}else{
	PreppedData$Streak_Probability[[u]] <- dpois(PreppedData$Win_Count[[u-1]]+1,
                                                PreppedData$Ave_Streak[[u]])
	      }
    }

write.csv(PreppedData, paste0("C:/Users/coloughlin/Documents/Temp/Update/Football Predictions/Data/Prepped/Prepped ",Input_Files[p]), row.names=F)
}
