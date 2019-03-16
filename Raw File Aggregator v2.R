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
    GW_Data <- Agg_Data[, j = list(max(Game_Week)), by = list(Season, Div)]

}
