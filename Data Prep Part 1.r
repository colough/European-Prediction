#----------- this file creates calculated variables for each country ----------#

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
#---------------------------Data Loading and Prep------------------------------#
################################################################################
i=1

file_location <- paste0("C:/Users/ciana/OneDrive/SONY_16M1/",
"Football Predictions/",Country[i],"/Raw Data/Aggregated")
setwd(file_location)
df <- read.csv(paste0("Aggregated_Raw_Data_",
Country[i],".csv"), header=T)


################################################################################
#---------------------------------Basic Calcs----------------------------------#
################################################################################

df$Home_Favourite <- ifelse(df$B365H < df$B365A & df$B365H < df$B365D,1,0)
df$Away_Favourite <- ifelse(df$B365A < df$B365H & df$B365A < df$B365D,1,0)
df$Favourite_Home_Win <- ifelse(df$B365H == pmin(df$B365H,df$B365D,df$B365A) &
                        df$Full_Time_Home_Goals == df$Full_Time_Away_Goals,1,0)
df$Favourite_Draw_Win <- ifelse(df$B365D == pmin(df$B365H,df$B365D,df$B365A) &
                        df$Full_Time_Home_Goals == df$Full_Time_Away_Goals,1,0)
