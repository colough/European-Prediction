#--------- this file aggregates the raw season files for each country ---------#

################################################################################
#-------------------------------Package Loading--------------------------------#
################################################################################
# Did Frodo get to Mount Doom and then realise he'd forgotten the ring?
# Course not, likewise remember your packages soldier

require(data.table)
require(lubridate)

################################################################################
#-----------------------------Function Creation--------------------------------#
################################################################################

# For the form function the variables need to be entered IN ORDER as follows:
# Form_Length,Season,Game_Week,Team_Result, HomeTeam(v/v), AwayTeam(v/v),
# Form_Var, Home_Form, df
Points_Form_Calc <- function(a,b,c,d,e,f,g,h,k){
    for(i in 1:nrow(k))
        if(c[i] > Form_Length)
            h[i] <- sum(k[b == b[i] & c >= (c[i] - a) & c < c[i] & d == e[i] &
                (e == e[i] | f == e[i]),eval(Form_Var)],
                k[b == b[i] & c >= (c[i] - a) & c < c[i] & d == "Draw" &
                    (e == e[i] | f == e[i]),eval(Form_Var)] )
                    else h[i] == 0
    return(h)
}

Form_Calc <- function(a,b,c,d,e,f,g,h,k){
    for(i in 1:nrow(k))
        if(c[i] > Form_Length)
            h[i] <- sum(k[b == b[i] & c >= (c[i] - a) & c < c[i] &
                e == e[i],eval(Form_Var1)],
                k[b == b[i] & c >= (c[i] - a) & c < c[i] &
                    f == e[i],eval(Form_Var2)])
                    else h[i] == 0
    return(h)
}

Average_Form_Calc <- function(a,b,c,d,e,f,g,h,k){
    for(i in 1:nrow(k))
        if(c[i] > Form_Length)
        h[i] <- ave(rbind(k[b == b[i] & c >= (c[i] - a) & c < c[i] &
            e == e[i],eval(Form_Var1)],
        k[b == b[i] & c >= (c[i] - a) & c < c[i] &
            f == e[i],eval(Form_Var2)]))
        else h[i] == 0
    return(h)
}

Ground_Average_Form_Calc <- function(a,b,c,d,e,f,g,h,k){
    for(i in 1:nrow(k))
        if(c[i] > Form_Length)
        h[i] <- mean(k[b == b[i] & c >= (c[i] - a) & c < c[i] &
            e == e[i],eval(Form_Var1)])
        else h[i] == 0
    return(h)
}

League_Ground_Average_Form_Calc <- function(a,b,c,d,e,f,g,h,k){
    for(i in 1:nrow(k))
        if(c[i] > Form_Length)
        h[i] <- mean(k[b == b[i] & c >= (c[i] - a) & c < c[i] ,eval(Form_Var1)])
        else h[i] == 0
    return(h)
}


AverageIF <- function(a,b,c,d,e,f,g,h,k){
    for(i in 1:nrow(k))
        if(c[i] > Form_Length)
        g[i] <- h[i] - ((h[i] - d) / nrow(k[b == b[i] & c >= (c[i] - a) &
        c < c[i] & (e == e[i] | f == e[i]),]))
        else g[i] <- 0
    return(g)
}

################################################################################
#-----------------------------Parameter Setting--------------------------------#
################################################################################

Form_Length <- 6
Country <- c("England", "France", "Spain", "Italy", "Germany")

################################################################################
#--------------------------------Data Loading----------------------------------#
################################################################################
for (i in 1:length(Country)){

  #file_location <- paste0("C:/Users/coloughlin/OneDrive/SONY_16M1/",
  #"Football Predictions/",Country[i],"/Raw Data/Aggregated")

  file_location <- paste0("C:/Users/ciana/OneDrive/SONY_16M1/",
  "Football Predictions/",Country[i],"/Raw Data/Aggregated")

  setwd(file_location)

  df <- read.csv(paste0("Aggregated_Raw_Data_",Country[i],".csv"), header=T)
  df <- as.data.table(df)

################################################################################
#---------------------------Calculated Variables-------------------------------#
################################################################################

  df$Home_Favourite <- ifelse(df$B365H < df$B365D & df$B365H < df$B365A, 1, 0)
  df$Away_Favourite <- ifelse(df$B365A < df$B365D & df$B365A < df$B365H, 1, 0)
  df[, Favourite_Home_Win := 0L]
  df[Home_Favourite == 1 & FTHG > FTAG,Favourite_Home_Win := 1L]
  df[, Favourite_Draw_Win := 0L]
  df[Home_Favourite == 0 & Away_Favourite == 0 & FTHG > FTAG,
                                              Favourite_Draw_Win := 1L]
  df[, Favourite_Away_Win := 0L]
  df[Away_Favourite == 1 & FTHG < FTAG,Favourite_Away_Win := 1L]
  df[, Outside_Home_Win := 0L]
  df[B365H > B365A & B365H > B365D & FTHG > FTAG,Outside_Home_Win := 1L]
  df[, Outside_Draw_Win := 0L]
  df[B365D > B365A & B365D > B365H & FTHG == FTAG,Outside_Draw_Win := 1L]
  df[, Outside_Away_Win := 0L]
  df[B365A > B365H & B365A > B365D & FTHG < FTAG,Outside_Away_Win := 1L]
  df[, Home_Win := 0L]
  df[FTR == "H",Home_Win := 1L]
  df[, Away_Win := 0L]
  df[FTR == "A",Away_Win := 1L]
  df[, Draw := 0L]
  df[FTR == "D",Draw := 1L]
  df[, Team_Result := "Draw"]
  df[Home_Win == 1,Team_Result := HomeTeam]
  df[Away_Win == 1,Team_Result := AwayTeam]
  df[,Points := 3L]
  df[Team_Result == "Draw",Points := 1L]
  df[,Home_Form := 0L]
  Form_Var <- quote(Points)
  df[, Home_Form := Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Form := 0L]
  Form_Var <- quote(Points)
  df[, Away_Form := Points_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df$D_No_B_Odds_Home <- df$B365H*(df$B365D-1)/df$B365D
  df$D_No_B_Odds_Away <- df$B365A*(df$B365D-1)/df$B365D
  df[, Winning_D_No_B_Odds := 1L]
  df[,Home_Win == 1, Winning_D_No_B_Odds := D_No_B_Odds_Home]
  df[,Away_Win == 1, Winning_D_No_B_Odds := D_No_B_Odds_Away]
  df[,Home_Shots_On_Target_Form := 0L]
  Form_Var1 <- quote(HST)
  Form_Var2 <- quote(AST)
  df[,Home_Shots_On_Target_Form := Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Shots_On_Target_Form := 0L]
  Form_Var2 <- quote(HST)
  Form_Var1 <- quote(AST)
  df[,Away_Shots_On_Target_Form := Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df$Relative_Shots_On_Target_Form <- df$Home_Shots_On_Target_Form -
                                      df$Away_Shots_On_Target_Form
  df[,Home_Shots_Conceded_Form := 0L]
  Form_Var2 <- quote(HST)
  Form_Var1 <- quote(AST)
  df[,Home_Shots_Conceded_Form := Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Shots_Conceded_Form := 0L]
  Form_Var1 <- quote(HST)
  Form_Var2 <- quote(AST)
  df[,Away_Shots_Conceded_Form := Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df$Relative_Shots_Conceded_Form <- df$Home_Shots_Conceded_Form -
                                      df$Away_Shots_Conceded_Form
  df[,Home_Goals_Scored_Form := 0L]
  Form_Var1 <- quote(FTHG)
  Form_Var2 <- quote(FTAG)
  df[,Home_Goals_Scored_Form := Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Goals_Scored_Form := 0L]
  Form_Var2 <- quote(FTHG)
  Form_Var1 <- quote(FTAG)
  df[,Away_Goals_Scored_Form := Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df$Relative_Goals_Scored_Form <- df$Home_Goals_Scored_Form -
                                      df$Away_Goals_Scored_Form
  df[,Home_Goals_Conceded_Form := 0L]
  Form_Var2 <- quote(FTHG)
  Form_Var1 <- quote(FTAG)
  df[,Home_Goals_Conceded_Form := Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Goals_Conceded_Form := 0L]
  Form_Var1 <- quote(FTHG)
  Form_Var2 <- quote(FTAG)
  df[,Away_Goals_Conceded_Form := Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df$Relative_Goals_Conceded_Form <- df$Away_Goals_Conceded_Form -
                                      df$Home_Goals_Conceded_Form
  df$Relative_Goals_Form <- df$Relative_Goals_Scored_Form +
                              df$Relative_Goals_Conceded_Form
  df$Home_Shot_Percentage <- df$HS / (df$HS + df$AS)
  df$Away_Shot_Percentage <- 1-df$Home_Shot_Percentage
  df[,Home_Shot_Percentage_Form := 0L]
  Form_Var1 <- quote(Home_Shot_Percentage)
  Form_Var2 <- quote(Away_Shot_Percentage)
  df[,Home_Shot_Percentage_Form := Average_Form_Calc(Form_Length,Season,
      Game_Week_Index,Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Shot_Percentage_Form := 0L]
  Form_Var2 <- quote(Home_Shot_Percentage)
  Form_Var1 <- quote(Away_Shot_Percentage)
  df[,Away_Shot_Percentage_Form := Average_Form_Calc(Form_Length,Season,
      Game_Week_Index,Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df[,Home_Shots_Conversion := 0.0]
  df$Home_Shots_Conversion <- as.double(df$Home_Shots_Conversion)
  df[HS > 0, Home_Shots_Conversion := FTHG / HS]
  df[,Away_Shots_Conversion := 0.0]
  df$Away_Shots_Conversion <- as.double(df$Away_Shots_Conversion)
  df[AS > 0, Away_Shots_Conversion := FTAG / AS]
  df[,Home_Shot_Conversion_Form := 0L]
  Form_Var1 <- quote(Home_Shots_Conversion)
  Form_Var2 <- quote(Away_Shots_Conversion)
  df[,Home_Shot_Conversion_Form := Average_Form_Calc(Form_Length,Season,
      Game_Week_Index,Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Shot_Conversion_Form := 0L]
  Form_Var2 <- quote(Home_Shots_Conversion)
  Form_Var1 <- quote(Away_Shots_Conversion)
  df[,Away_Shot_Conversion_Form := Average_Form_Calc(Form_Length,Season,
      Game_Week_Index,Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df[,Home_Conversion_Offset := 0.0]
  df$Home_Conversion_Offset <- as.double(df$Home_Conversion_Offset)
  df[Home_Shot_Conversion_Form > 0, Home_Conversion_Offset :=
                              Home_Shots_Conversion / Home_Shot_Conversion_Form]
  df[,Away_Conversion_Offset := 0.0]
  df$Away_Conversion_Offset <- as.double(df$Away_Conversion_Offset)
  df[Away_Shot_Conversion_Form > 0, Away_Conversion_Offset :=
                              Away_Shots_Conversion / Away_Shot_Conversion_Form]
  df[,Home_Conversion_Offset_Form := 0L]
  Form_Var1 <- quote(Home_Conversion_Offset)
  Form_Var2 <- quote(Away_Conversion_Offset)
  df[,Home_Conversion_Offset_Form := Average_Form_Calc(Form_Length,Season,
      Game_Week_Index,Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Conversion_Offset_Form := 0L]
  Form_Var2 <- quote(Home_Conversion_Offset)
  Form_Var1 <- quote(Away_Conversion_Offset)
  df[,Away_Conversion_Offset_Form := Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df$Goal_Difference <- df$FTHG - df$FTAG
  df[, Home_Points := 1L]
  df[Home_Win == 1, Home_Points := 3]
  df[Away_Win == 1, Home_Points := 0]
  df[, Away_Points := 1L]
  df[Away_Win == 1, Away_Points := 3]
  df[Home_Win == 1, Away_Points := 0]
  df[,Home_Attacking_Form := 0L]
  Form_Var1 <- quote(FTHG)
  Form_Var2 <- quote(FTAG)
  df[,Home_Attacking_Form := Ground_Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Attacking_Form := 0L]
  Form_Var1 <- quote(FTAG)
  Form_Var2 <- quote(FTAG)
  df[,Away_Attacking_Form := Ground_Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df[,Home_Defensive_Form := 0L]
  Form_Var1 <- quote(FTAG)
  Form_Var2 <- quote(FTAG)
  df[,Home_Defensive_Form := Ground_Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,Away_Defensive_Form := 0L]
  Form_Var1 <- quote(FTHG)
  Form_Var2 <- quote(FTAG)
  df[,Away_Defensive_Form := Ground_Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df[,League_Home_Attacking_Form := 0L]
  Form_Var1 <- quote(FTHG)
  Form_Var2 <- quote(FTAG)
  df[,League_Home_Attacking_Form := League_Ground_Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,League_Away_Attacking_Form := 0L]
  Form_Var1 <- quote(FTAG)
  Form_Var2 <- quote(FTAG)
  df[,League_Away_Attacking_Form := League_Ground_Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df[,League_Home_Defensive_Form := 0L]
  Form_Var1 <- quote(FTAG)
  Form_Var2 <- quote(FTAG)
  df[,League_Home_Defensive_Form := League_Ground_Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, HomeTeam, AwayTeam, Form_Var, Home_Form, df)]
  df[,League_Away_Defensive_Form := 0L]
  Form_Var1 <- quote(FTHG)
  Form_Var2 <- quote(FTAG)
  df[,League_Away_Defensive_Form := League_Ground_Average_Form_Calc(Form_Length,Season,Game_Week_Index,
                      Team_Result, AwayTeam, HomeTeam, Form_Var, Home_Form, df)]
  df[, Home_Attack_Strength := 0L]
  df$Home_Attack_Strength <- as.double(df$Home_Attack_Strength)
  df[League_Home_Attacking_Form > 0, Home_Attack_Strength := Home_Attacking_Form
                                                      / League_Home_Attacking_Form]
  df[, Away_Attack_Strength := 0L]
  df$Away_Attack_Strength <- as.double(df$Away_Attack_Strength)
  df[League_Away_Attacking_Form > 0, Away_Attack_Strength := Away_Attacking_Form
                                                      / League_Away_Attacking_Form]
  df[, Home_Defense_Strength := 0L]
  df$Home_Defense_Strength <- as.double(df$Home_Defense_Strength)
  df[League_Home_Defensive_Form > 0, Home_Defense_Strength := Home_Defensive_Form
                                                      / League_Home_Defensive_Form]
  df[, Away_Defense_Strength := 0L]
  df$Away_Defense_Strength <- as.double(df$Away_Defense_Strength)
  df[League_Away_Defensive_Form > 0, Away_Defense_Strength := Away_Defensive_Form
                                                      / League_Away_Defensive_Form]
  df$Home_Likely_Goals <- df$League_Home_Attacking_Form*df$Home_Attack_Strength*
                              df$Away_Defense_Strength
  df$Away_Likely_Goals <- df$League_Away_Attacking_Form*df$Away_Attack_Strength*
                              df$Home_Defense_Strength
  df$Home_Poisson_0 <- ppois(0,df$Home_Likely_Goals)
  df$Home_Poisson_1 <- ppois(1,df$Home_Likely_Goals,FALSE)
  df$Home_Poisson_2 <- ppois(2,df$Home_Likely_Goals,FALSE)
  df$Home_Poisson_3 <- ppois(3,df$Home_Likely_Goals,FALSE)
  df$Home_Poisson_4 <- ppois(4,df$Home_Likely_Goals,FALSE)
  df$Home_Poisson_5 <- ppois(5,df$Home_Likely_Goals,FALSE)
  df$Away_Poisson_0 <- ppois(0,df$Away_Likely_Goals)
  df$Away_Poisson_1 <- ppois(1,df$Away_Likely_Goals, FALSE)
  df$Away_Poisson_2 <- ppois(2,df$Away_Likely_Goals, FALSE)
  df$Away_Poisson_3 <- ppois(3,df$Away_Likely_Goals, FALSE)
  df$Away_Poisson_4 <- ppois(4,df$Away_Likely_Goals, FALSE)
  df$Away_Poisson_5 <- ppois(5,df$Away_Likely_Goals, FALSE)
  df[, Home_Poisson_Win := rowSums(cbind(df$Home_Poisson_1*df$Away_Poisson_0,
            df$Home_Poisson_2*df$Away_Poisson_0, df$Home_Poisson_2*
            df$Away_Poisson_1,df$Home_Poisson_3*df$Away_Poisson_0,
            df$Home_Poisson_3*df$Away_Poisson_1,df$Home_Poisson_3*
            df$Away_Poisson_2,df$Home_Poisson_4*df$Away_Poisson_0,
            df$Home_Poisson_4*df$Away_Poisson_1,df$Home_Poisson_4*
            df$Away_Poisson_2,df$Home_Poisson_4*df$Away_Poisson_3,
            df$Home_Poisson_5*df$Away_Poisson_0,df$Home_Poisson_5*
            df$Away_Poisson_1,df$Home_Poisson_5*df$Away_Poisson_2,
            df$Home_Poisson_5*df$Away_Poisson_3,df$Home_Poisson_5*
            df$Away_Poisson_4))]
    df[, Home_Poisson_Draw := rowSums(cbind(df$Home_Poisson_0*df$Away_Poisson_0,
              df$Home_Poisson_1*df$Away_Poisson_1, df$Home_Poisson_2*
              df$Away_Poisson_2,df$Home_Poisson_3*df$Away_Poisson_3,
              df$Home_Poisson_4*df$Away_Poisson_4,df$Home_Poisson_5*
              df$Away_Poisson_5))]
    df[, Away_Poisson_Win := rowSums(cbind(df$Away_Poisson_1*df$Home_Poisson_0,
              df$Away_Poisson_2*df$Home_Poisson_0, df$Away_Poisson_2*
              df$Home_Poisson_1,df$Away_Poisson_3*df$Home_Poisson_0,
              df$Away_Poisson_3*df$Home_Poisson_1,df$Away_Poisson_3*
              df$Home_Poisson_2,df$Away_Poisson_4*df$Home_Poisson_0,
              df$Away_Poisson_4*df$Home_Poisson_1,df$Away_Poisson_4*
              df$Home_Poisson_2,df$Away_Poisson_4*df$Home_Poisson_3,
              df$Away_Poisson_5*df$Home_Poisson_0,df$Away_Poisson_5*
              df$Home_Poisson_1,df$Away_Poisson_5*df$Home_Poisson_2,
              df$Away_Poisson_5*df$Home_Poisson_3,df$Away_Poisson_5*
              df$Home_Poisson_4))]
    df[, Home_Form_If_Win := 0]
    df[, Home_Form_If_Win := AverageIF(Form_Length,Season,Game_Week_Index,
                        3, HomeTeam, AwayTeam, Home_Form_If_Win, Home_Form, df)]
    df[, Home_Form_If_Draw := 0]
    df[, Home_Form_If_Draw := AverageIF(Form_Length,Season,Game_Week_Index,
                        1, HomeTeam, AwayTeam, Home_Form_If_Win, Home_Form, df)]
    df[, Home_Form_If_Lose := 0]
    df[, Home_Form_If_Lose := AverageIF(Form_Length,Season,Game_Week_Index,
                        0, HomeTeam, AwayTeam, Home_Form_If_Win, Home_Form, df)]



}
