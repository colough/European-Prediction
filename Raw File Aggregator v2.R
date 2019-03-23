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
Game_Week_Goodies <- read.csv("GameWeek_Lookups.csv", header = T)
Game_Week_Goodies$Date <- lubridate::dmy(Game_Week_Goodies$Date)
Team_Tier_Goodies <- read.csv("Team_Tier_Lookups.csv", header = T)

###############################################################################
#----------------------------Function Creation--------------------------------#
###############################################################################

GW_Calc_1 <- function(a, b, c) {
    # a is season, b is Date and c is GW1
    for (i in 2:(length(c) - 1))
        if (a[i] == a[i - 1])
            if (b[i] >= b[i - 1] + 2 & b[i] >= b[i + 1] - 2)
                c[i] = c[i - 1] + 1
            else c[i] = c[i - 1]
            else c[i] == 1
            return(c)
}

MN_Calc <- function(a, b) {
    # a is GW, b is MN
    for (i in 2:length(a))
        if (a[i] == a[i - 1])
            b[i] = b[i - 1] + 1
        else b[i] == 1
        return(b)
}

GW_Calc_2 <- function(a, b, c,d) {
    # a is Game_Week, b is Max_MN, c is Match_Number_1
    for (i in 2:length(a))
        if (d[i] != d[i - 1])
            a[i] = 1
                else if (b[i] < 6)
                        a[i] = a[i - 1]
                    else if (b[i] >= 6 & c[i] == 1)
                        a[i] = a[i - 1] + 1
                    else a[i] = a[i - 1]
                    return(a)
}


###############################################################################
#------------------------------Data Aggregation-------------------------------#
###############################################################################

# ok loop through and glue everything together
for (i in 1:length(Country)) {
    # Where are the files located?
    file_location <- paste0("C:/Users/ciana/OneDrive/SONY_16M1/",
  "Football Predictions/", Country[i], "/Raw Data")
    setwd(file_location)
    file_list <- list.files()
    file_list <- file_list[file_list != "Aggregated"]
    # Spoiler Alert: We'll need this later
    Agg_Data <- data.frame()

#-------------------------- Aggregate the raw files --------------------------#
    for (j in 1:length(file_list)) {
        df <- read.csv(file_list[j], header = TRUE)
        Agg_Data <- rbindlist(list(Agg_Data, df), use.names = TRUE, fill = TRUE)
    }

    Agg_Data$Date <- lubridate::dmy(Agg_Data$Date)
    # a couple of NAs pop up so let's remove them:
    Agg_Data <- Agg_Data[complete.cases(Agg_Data$FTHG),]
    Agg_Data <- as.data.table(Agg_Data)

#---------------------------- Add in Game Week Info --------------------------#
    # Sort the data by division and date
    Agg_Data <- setorder(Agg_Data, Div, Date)
    # Add in the Season Indicator
    Agg_Data[, Month := lubridate::month(Agg_Data$Date)]
    Agg_Data[, Season := ""]
    Agg_Data$Season <- ifelse(Agg_Data$Month > 6, paste0(
    as.character(lubridate::year(Agg_Data$Date)), " ",
    as.character(lubridate::year(Agg_Data$Date + 365))),
    paste0(
    as.character(lubridate::year(Agg_Data$Date - 365)), " ",
    as.character(lubridate::year(Agg_Data$Date))))
    # Populate initial game weeks
    Agg_Data[, GW_1 := 1]
    Agg_Data[, Match_Number_1 := 1]
    Agg_Data[, GW_1 := GW_Calc_1(Season, Date, GW_1)]
    Agg_Data[, Match_Number_1 := MN_Calc(GW_1, Match_Number_1)]
    # Create the max MN's for each GW
    Max_Match_Numbers <- Agg_Data[, j = list(Max_MN = max(Match_Number_1)),
                                    by = list(Season, Div, GW_1)]
    # Join back to the main table
    setDT(Agg_Data)[Max_Match_Numbers, Max_MN := i.Max_MN,
    on = c("Season" = "Season", "Div" = "Div", "GW_1" = "GW_1")]
    Agg_Data[, Game_Week := 1]
    Agg_Data[, Game_Week := GW_Calc_2(Game_Week,
                            Max_MN, Match_Number_1, Season)]

#---------------------------- Create the Tier Info ---------------------------#
    #GW_Data <- Agg_Data[, j = list(max(Game_Week)), by = list(Season, Div)]
    Tier_Cols <- c("Date","Div","HomeTeam","AwayTeam","FTR")
    Tier <- Agg_Data[,Tier_Cols, with=FALSE]
    # Create a new table thet is going to have the results and then the points
    Tier[, Home_Win := 0L]
    Tier[FTR == "H", Home_Win := 1L]
    Tier[, Away_Win := 0L]
    Tier[FTR == "A", Away_Win := 1L]
    Tier[, Draw := 0L]
    Tier[FTR == "D", Draw := 1L]
    Tier[, Team_Result := "Draw"]
    Tier[Home_Win == 1, Team_Result := HomeTeam]
    Tier[Away_Win == 1, Team_Result := AwayTeam]
    Tier[, Points := 3L]
    Tier[Team_Result == "Draw", Points := 1L]
    Tier[, Home_Points := 1L]
    Tier[Home_Win == 1, Home_Points := 3]
    Tier[Away_Win == 1, Home_Points := 0]
    Tier[, Away_Points := 1L]
    Tier[Away_Win == 1, Away_Points := 3]
    Tier[Home_Win == 1, Away_Points := 0]

    # Combine the division and the team
    Tier[, DivHTeam := paste0(Div, HomeTeam)]
    Tier[, DivATeam := paste0(Div, AwayTeam)]
    # Create points
    TierHP <- Tier[, j = list(Sum_HP = sum(Home_Points),
                            Num_Matches = .N), by = list(DivHTeam)]
    TierAP <- Tier[, j = list(Sum_AP = sum(Away_Points),
                            Num_AMatches = .N), by = list(DivATeam)]
    # Join and aggregate up by team
    Tier_Points <- setDT(TierHP)[TierAP, Sum_AP :=
                            i.Sum_AP, on = c("DivHTeam" = "DivATeam")]
    Tier_Points <- setDT(Tier_Points)[TierAP, Num_AMatches :=
                            i.Num_AMatches, on = c("DivHTeam" = "DivATeam")]
    Tier_Points$Total_Points <- Tier_Points$Sum_AP + Tier_Points$Sum_HP
    Tier_Points$Total_Matches <- Tier_Points$Num_Matches +
                                        Tier_Points$Num_AMatches
    Tier_Points$Average_Total_Points <- Tier_Points$Total_Points /
                                    Tier_Points$Total_Matches
    Tier_Points$Average_HPoints <- Tier_Points$Sum_HP /
                                    Tier_Points$Num_Matches
    Tier_Points$Average_APoints <- Tier_Points$Sum_AP /
                                    Tier_Points$Num_AMatches
   
    # Classify them
    Tier_Points$Total_Tier <- cut(Tier_Points$Average_Total_Points,
                        breaks = quantile(Tier_Points$Average_Total_Points),
                        labels = 4:1, include.lowest=T)
    Tier_Points$Home_Tier <- cut(Tier_Points$Average_HPoints,
                        breaks = quantile(Tier_Points$Average_HPoints),
                        labels = 4:1, include.lowest = T)
    Tier_Points$Away_Tier <- cut(Tier_Points$Average_APoints,
                        breaks = quantile(Tier_Points$Average_APoints),
                        labels = 4:1, include.lowest = T)
    # Join back to Tier
    setDT(Tier)[Tier_Points, Home_Team_Tier := i.Home_Tier,
    on = c("DivHTeam" = "DivHTeam")]
    setDT(Tier)[Tier_Points, Away_Team_Tier := i.Away_Tier,
    on = c("DivATeam" = "DivHTeam")]
    # Calculate the Tier summary stats
    # The below two creations are based off the individual games,but that 
    # gives a little too much swing for my liking so I cut it down to just
    # the second two which is based on summaries of the tiered teams
    Team_Tier_Goodies_H <- Tier[, j = list(Home_Tier_Avg_Points =
                                mean(Home_Points), Home_Tier_St_Dev =
                                sd(Home_Points)), by = list(Home_Team_Tier)]
    Team_Tier_Goodies_A <- Tier[, j = list(Away_Tier_Avg_Points =
                                mean(Away_Points), Away_Tier_St_Dev =
                                sd(Away_Points)), by = list(Away_Team_Tier)]

    Team_Tier_Goodies_H1 <- Tier_Points[, j = list(Home_Tier_Avg_Points =
                                mean(Average_HPoints), Home_Tier_St_Dev =
                                sd(Average_HPoints)), by = list(Home_Tier)]
    Team_Tier_Goodies_A1 <- Tier_Points[, j = list(Away_Tier_Avg_Points =
                                mean(Average_APoints), Away_Tier_St_Dev =
                                sd(Average_APoints)), by = list(Away_Tier)]
    # Join back to Tier table
    setDT(Tier)[Team_Tier_Goodies_H1, Home_Tier_Avg_Points :=
        i.Home_Tier_Avg_Points, on = c("Home_Team_Tier" = "Home_Tier")]
    setDT(Tier)[Team_Tier_Goodies_H1, Home_Tier_St_Dev :=
        i.Home_Tier_St_Dev, on = c("Home_Team_Tier" = "Home_Tier")]
    setDT(Tier)[Team_Tier_Goodies_A1, Away_Tier_Avg_Points :=
        i.Away_Tier_Avg_Points, on = c("Away_Team_Tier" = "Away_Tier")]
    setDT(Tier)[Team_Tier_Goodies_A1, Away_Tier_St_Dev :=
        i.Away_Tier_St_Dev, on = c("Away_Team_Tier" = "Away_Tier")]

    # Aaaaand back to Agg_Data:
    setDT(Agg_Data)[Tier, Home_Tier_Avg_Points :=
        i.Home_Tier_Avg_Points, on = c("Date" = "Date", 
        "HomeTeam = HomeTeam", "AwayTeam = AwayTeam")]
    setDT(Agg_Data)[Tier, Home_Tier_St_Dev :=
        i.Home_Tier_St_Dev, on = c("Date" = "Date",
        "HomeTeam = HomeTeam", "AwayTeam = AwayTeam")]
    setDT(Agg_Data)[Tier, Away_Tier_Avg_Points :=
        i.Away_Tier_Avg_Points, on = c("Date" = "Date",
        "HomeTeam = HomeTeam", "AwayTeam = AwayTeam")]
    setDT(Agg_Data)[Tier, Away_Tier_St_Dev :=
        i.Away_Tier_St_Dev, on = c("Date" = "Date",
        "HomeTeam = HomeTeam", "AwayTeam = AwayTeam")]
    setDT(Agg_Data)[Tier, Home_Team_Tier :=
        i.Home_Team_Tier, on = c("Date" = "Date",
        "HomeTeam = HomeTeam", "AwayTeam = AwayTeam")]
    setDT(Agg_Data)[Tier, Away_Team_Tier :=
        i.Away_Team_Tier, on = c("Date" = "Date",
        "HomeTeam = HomeTeam", "AwayTeam = AwayTeam")]
}
