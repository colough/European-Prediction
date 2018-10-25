#---- this file creates an XGBoost classification model for European data ----#

###############################################################################
#------------------------------Package Loading--------------------------------#
###############################################################################
# Like Voldemort and his Horcruxes we're a little dependent on our packages..

require(pls)
require(data.table)
require(RSNNS)
require(plyr)
require(caret)
require(mlr)
require(parallelMap)
require(rgenoud)
require(DiceKriging)
require(parallelMap)
require(mlrMBO)
require(devtools)
require(vtreat)
require(SuperLearner)

###############################################################################
#-----------------------------Parameter Settings------------------------------#
###############################################################################
# what Season are we predicting? (Enter numeric)
Season_prediction <- 20142015
# Are we doing a single market or Europe wide?
# Take away any leagues you don't want included:
# Full List: League <- c('D1','E0', 'F1', 'SP1', 'I1')
League <- c('D1', 'E0', 'F1', 'SP1', 'I1')

# How many games in a season?
GWRange <- 38 #- 38 games in a season son

###############################################################################
#--------------------------------Data Loading---------------------------------#
###############################################################################

# which project folder we want to work in
setwd(paste0("C:/Users/ciana/OneDrive/SONY_16M1/Football Predictions/",
      "Europe/Output Data"))
df <- read.csv("Europe Prepped Output.csv", header = TRUE)
df <- as.data.table(df)
#df <- df[complete.cases(df),]

#--------------------------- Apply Seasonal Filters --------------------------#

# convert to numeric
df$Season <- gsub(" ", "", df$Season)
df$Season <- as.numeric(df$Season)
df <- df[Season <= Season_prediction,]

#---------------------------- Apply Market Filters ---------------------------#
df <- df[Div %in% League]

#----------------------------- Apply Team Filters ----------------------------#
# Only want to build models for teams who are in current season
Teams <- unique(df[Season == max(df$Season), HomeTeam])
# Can only take teams whose first season isn't the one currently predicting:
# So create unique list of Seasons and teams and then look at all teams with
# greater than one row
df$HomeTeam <- as.character(df$HomeTeam)
Temp_Teams <- as.data.frame(unique(df[HomeTeam %in% Teams, c("HomeTeam",
                                                    "Season")]))
Temp_Teams <- as.data.table(Temp_Teams)
Team_Count <- Temp_Teams[, .N, by = HomeTeam]
Team_Count <- Team_Count[N > 1]
Teams <- Team_Count[, c("HomeTeam")]
df <- df[df$HomeTeam %in% Teams$HomeTeam,]
df <- df[df$AwayTeam %in% Teams$HomeTeam,]

# Create empty containers to hold our results later:
PredResults <- data.frame()
StatResults <- data.frame()
###############################################################################
#-------------------------------Model Building--------------------------------#
###############################################################################

#------------------------ Loop through every gameweek ------------------------#
#for (i in 8:GWRange) {

#------------------ Define and transform model training set ------------------#

