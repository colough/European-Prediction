#--------- this file aggregates the raw season files for each country ---------#

################################################################################
#-------------------------------Package Loading--------------------------------#
################################################################################
# Did Frodo get to Mount Doom and then realise he'd forgotten the ring?
# Course not, likewise remember your packages soldier

require(data.table)
require(lubridate)
################################################################################
#-=----------------------------Parameter Setting-------------------------------#
################################################################################
# That one equals above is an OCD test. How'd you do?
# what countries are to be included?
Country <- c("England", "France", "Spain", "Italy", "Germany")

################################################################################
#------------------------------Data Aggregation--------------------------------#
################################################################################

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
    for (j in 1:length(file_list)){
        df <- read.csv(file_list[j], header=TRUE)
        Agg_Data <- rbindlist(list(Agg_Data,df), use.names=TRUE, fill=TRUE)
    }
    Agg_Data$Date <- lubridate::dmy(Agg_Data$Date)
    write.csv(Agg_Data, paste0("Aggregated/Aggregated_Raw_Data_",
    Country[i],".csv"),row.names=F)
}
# tidy
