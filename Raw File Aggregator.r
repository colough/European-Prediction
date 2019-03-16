#--------- this file aggregates the raw season files for each country --------#

###############################################################################
#-------------------------------Package Loading-------------------------------#
###############################################################################
# Did Frodo get to Mount Doom and then realise he'd forgotten the ring?
# Course not, likewise remember your packages soldier

require(data.table)
require(lubridate)

###############################################################################
#-=----------------------------Parameter Setting------------------------------#
###############################################################################
# That one equals above is an OCD test. How'd you do?
# what countries are to be included?
Country <- c("England", "France", "Spain", "Italy", "Germany")

# Load up the data to be merged to the aggregated files:
Treasure_Map <- paste0("C:/Users/ciana/OneDrive/SONY_16M1/",
"Football Predictions/Europe/Input Data")
setwd(Treasure_Map)
Game_Week_Goodies <- read.csv("GameWeek_Lookups.csv", header=T)
Game_Week_Goodies$Date <- lubridate::dmy(Game_Week_Goodies$Date)
Team_Tier_Goodies <- read.csv("Team_Tier_Lookups.csv", header=T)

###############################################################################
#------------------------------Data Aggregation-------------------------------#
###############################################################################

# ok loop through and glue everything together
for (i in 1:length(Country)){
  # Where are the files located?
  file_location <- paste0("C:/Users/ciana/OneDrive/SONY_16M1/",
  "Football Predictions/",Country[i],"/Raw Data")
  setwd(file_location)
  file_list <- list.files()
  file_list <- file_list[file_list != "Aggregated"]
  # Spoiler Alert: We'll need this later
  Agg_Data <- data.frame()
#-------------------------- Aggregate the raw files --------------------------#
  for (j in 1:length(file_list)){
    df <- read.csv(file_list[j], header=TRUE)
    Agg_Data <- rbindlist(list(Agg_Data,df), use.names=TRUE, fill=TRUE)
  }

  Agg_Data$Date <- lubridate::dmy(Agg_Data$Date)
  # a couple of NAs pop up so let's remove them:
  Agg_Data <- Agg_Data[complete.cases(Agg_Data$FTHG),]
  Agg_Data <- as.data.table(Agg_Data)
#---------------------------- Add in Game Week Info --------------------------#
  Agg_Data <- setDT(Agg_Data)[Game_Week_Goodies, Match_index :=
  i.Match_index, on = c("HomeTeam" = "HomeTeam", "AwayTeam" = "AwayTeam",
  "Date" = "Date")]

  Agg_Data <- setDT(Agg_Data)[Game_Week_Goodies, Game_Week_Index :=
  i.Game_Week_Index, on = c("HomeTeam" = "HomeTeam", "AwayTeam" = "AwayTeam",
  "Date" = "Date")]

  Agg_Data <- setDT(Agg_Data)[Game_Week_Goodies, Season :=
  i.Season, on = c("HomeTeam" = "HomeTeam", "AwayTeam" = "AwayTeam",
  "Date" = "Date")]
  # Pop these guys to the front of the data frame
  setcolorder(Agg_Data, c(ncol(Agg_Data):(ncol(Agg_Data)-2),
  1:(ncol(Agg_Data)-3)))
#---------------------------- Add in Team Tier Info --------------------------#
  Agg_Data <- setDT(Agg_Data)[Team_Tier_Goodies, Home_Team_Tier :=
  i.Tier, on = c("HomeTeam" = "Team")]

  Agg_Data <- setDT(Agg_Data)[Team_Tier_Goodies, Home_Tier_Avg_Points :=
  i.Home_Tier_Avg_Points, on = c("HomeTeam" = "Team")]

  Agg_Data <- setDT(Agg_Data)[Team_Tier_Goodies, Home_Tier_St_Dev :=
  i.Home_Tier_St_Dev, on = c("HomeTeam" = "Team")]

  Agg_Data <- setDT(Agg_Data)[Team_Tier_Goodies, Away_Team_Tier :=
  i.Tier, on = c("AwayTeam" = "Team")]

  Agg_Data <- setDT(Agg_Data)[Team_Tier_Goodies, Away_Tier_Avg_Points :=
  i.Away_Tier_Avg_Points, on = c("AwayTeam" = "Team")]

  Agg_Data <- setDT(Agg_Data)[Team_Tier_Goodies, Away_Tier_St_Dev :=
  i.Away_Tier_St_Dev, on = c("AwayTeam" = "Team")]
  Agg_Data <- Agg_Data[complete.cases(Agg_Data$Game_Week_Index),]
  #------------------------------ Wrap it in a bow ------------------------------#
  write.csv(Agg_Data, paste0("Aggregated/Aggregated_Raw_Data_",
  Country[i],".csv"),row.names=F)
}
# tidy