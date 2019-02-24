#- this file creates an Random Forest classification model for European data -#

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
require(mlrMBO)
#require(devtools)
require(vtreat)

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

# The pot of gold at the end of the rainbow is called:
Pot <- "Europe Calc Data Classification Output.csv"

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

for (i in 8:GWRange) {

    #------------------ Define and transform model training set ------------------#
    ModTrain1 <- df[Season < Season_prediction,]
    ModTrain2 <- df[Season == Season_prediction & Game_Week_Index < i,]
    ModTrain <- rbindlist(list(ModTrain1, ModTrain2))
    ModTrain <- ModTrain[Game_Week_Index >= 7,]
    # we'll create a couple of extra variables to make things a little
    # easier for the model here each time
    ModTrain$Relative_Form <- ModTrain$Team_Form -
                        ModTrain$Opposition_Form
    ModTrain$Relative_Odds <- ModTrain$Opposition_Odds -
                        ModTrain$Team_Odds
    # Turn Seasons back into a factor so we can use it in the model
    ModTrain$Season <- as.factor(ModTrain$Season)
    # Like Han Solo, we have to do the odd bit of smuggling every now and then
    # in this case it's abducting the dependent in it's numeric form so that
    # we can mean encode the team variable
    Team_Dependent <- data.frame(ModTrain$Team_Goal_Diff)
    # As we're doing a classification model we want the dependent to be
    # classes
    ModTrain$Team_Goal_Diff <- ifelse(ModTrain$Team_Goal_Diff > 0, 2,
                    ifelse(ModTrain$Team_Goal_Diff < 0, 0, 1))
    ModTrain$Team_Goal_Diff <- as.factor(ModTrain$Team_Goal_Diff)
    ModTrain <- as.data.frame(ModTrain)
    # Define the variables to be used and then create numeric dummies
    variables <- c('Season', 'Team_Favourite', 'Team_Shots_Conceded_Form',
    'Opposition_Shots_Conceded_Form', 'Team_Goals_Scored_Form',
    'Opposition_Goals_Scored_Form', 'Home_Away', 'Match_Tier',
    'Team_Goals_Conceded_Form', 'Opposition_Goals_Conceded_Form',
    'Team_Odds', 'Opposition_Odds', 'Poisson_Result', 'Regress_Result',
    'Team_Handicap', 'Relative_Odds', 'Relative_Form')
    TDat1 <- ModTrain[, variables]
    TDat2 <- dummyVars("~.", data = TDat1)
    TrainDat <- data.frame(predict(TDat2, newdata = TDat1))
    #- now we need to normalize the training data
    TrainDatnames <- colnames(TrainDat)
    #TrainDat <- normalizeData(TrainDat, type="0_1")
    colnames(TrainDat) <- TrainDatnames

    # Get the mean encoded team variables
    TDat3 <- ModTrain
    TDat3 <- cbind(TDat3, Team_Dependent)
    colnames(TDat3)[ncol(TDat3)] <- "Dependent"
    Team_variables <- c('Team', 'Opposition')
    cfe <- vtreat::mkCrossFrameNExperiment(TDat3, Team_variables, "Dependent")
    plan <- cfe$treatments
    Team_Vars <- cfe$crossFrame
    codes <- c('lev', 'catN', 'clean', 'isBAD')
    Team_Vars <- Team_Vars[, grep(paste(codes, collapse = "|"),
                                colnames(Team_Vars), value = TRUE)]

    # Add in the mean encoded variables
    TrainDat <- cbind(TrainDat, Team_Vars)
    # join the Dependent variable
    Dependent <- data.frame(ModTrain$Team_Goal_Diff)
    TrainDat <- cbind(TrainDat, Dependent)

    # ok so to start modelling we have to declare a so called 'task', with the
    # dependent and independent variables called out
    TrainDat <- as.data.frame(TrainDat)
    TrainDat <- TrainDat[complete.cases(TrainDat),]
    trainTask <- makeClassifTask(data = TrainDat,
                    target = "ModTrain.Team_Goal_Diff")

    Agg_Results <- data.frame()

    #---------------- Gradient Boosting (Create prediction data) -----------------#

    # The below line looks like a stupid mistake on my part but actually it's a
    # deliberate mistake to counter a stupid mistake on R's part
    PredData <- df[Game_Week_Index == i,]

    # easier for the model here each time
    PredData$Relative_Form <- PredData$Team_Form -
                        PredData$Opposition_Form
    PredData$Relative_Odds <- PredData$Opposition_Odds -
                        PredData$Team_Odds
    # Turn Seasons back into a factor so we can use it in the model
    PredData$Season <- as.factor(PredData$Season)
    PredData <- as.data.frame(PredData)

    PDat1 <- PredData[, variables]
    PDat2 <- dummyVars("~.", data = PDat1)
    PredDat <- data.frame(predict(PDat2, newdata = PDat1))
    # now we need to normalize the prediction data
    PredDatnames <- colnames(PredDat)
    #PredDat <- normalizeData(PredDat, type="0_1")
    colnames(PredDat) <- PredDatnames

    # And the team variables don't forget!
    Pred_Team_Vars <- vtreat::prepare(plan, PredData, pruneSig = NULL)
    PredDat <- cbind(PredDat, Pred_Team_Vars)

    # Now I have to dynamically filter to make up for that R mistake
    # Just to be clear: R's mistake. Definitely not mine
    Season_Col <- paste0("Season.", Season_prediction)
    Cal_Season_Col <- as.character(unique(df[Season == Season_prediction &
                Game_Week_Index == i, Calendar_Season]))
    PredDat <- PredDat[PredDat[, eval(Season_Col)] == 1,]

    #PredDat <- as.matrix(PredDat)
    #PredDat <- xgboost::xgb.DMatrix(PredDat)

    #----------------- Random Forest (Hyperparameter Tuning) -----------------#

    # When doing Hyperparameter tuning we need to save the model in a local
    # folder so we temporarily move to the below
    setwd("C:/Users/ciana/Documents/Football Predictions/Europe")

    parallelStartSocket(3)
    ptm <- proc.time()
    # Let's make a learner. Together.
    Rf_mod <- makeLearner("classif.randomForest", predict.type = "prob")

    # refine the parameter values
    ps = makeParamSet(
    makeIntegerParam("ntree", lower = 50, upper = 700),
    makeIntegerParam("mtry", lower = 3, upper = 15),
    makeIntegerParam("nodesize", lower = 10, upper = 50)
    )
    ctrl = makeTuneControlMBO()
    inner = makeResampleDesc("Subsample", iters = 3)
    # Tuning in Inner resampling loop
    #lrn = makeTuneWrapper("classif.xgboost", resampling = inner, par.set = ps,
    #                            control = ctrl, show.info = FALSE)

    # tuning in Outer resampling loop
    outer = makeResampleDesc("CV", iters = 3)
    #r = resample(lrn, trainTask, resampling = outer, extract = getTuneResult,
    #                                                    show.info = FALSE)

    Params <- tuneParams(Rf_mod, task = trainTask, par.set = ps,
                        resampling = outer, control = ctrl, measures = acc)
    parallelStop()
    proc.time() - ptm
    # Bring it back
    setwd(paste0("C:/Users/ciana/OneDrive/SONY_16M1/Football Predictions/",
          "Europe/Output Data"))

    #----------------- Gradient Boosting (build best model type) -----------------#

    Rf_tuned <- setHyperPars(learner = Rf_mod, par.vals = Params$x)
    Rfmodel <- train(Rf_tuned, trainTask)

    #--------------- Gradient Boosting (Prediction & Context data) ---------------#

    # Fit is a column of our predicted values
    # Act is the actual result in terms of goal difference
    Fit <- predict(Rfmodel, newdata = PredDat)
    Fit <- as.data.table(Fit)

    P_Draw <- Fit$prob.1
    P_Opposition <- Fit$prob.0
    P_Team <- Fit$prob.2

    PredData$Season <- as.numeric(PredData$Season)
    PredData <- as.data.table(PredData)

    div1 <- df[Season == Season_prediction & Game_Week_Index == i, Div]
    div1 <- as.data.frame(div1)
    p1 <- df[Season == Season_prediction & Game_Week_Index == i, Team]
    p1 <- as.data.frame(p1)
    p2 <- rep(Season_prediction, nrow(PredDat))
    p2 <- as.data.frame(p2)
    p3 <- df[Season == Season_prediction & Game_Week_Index == i, Opposition]
    p3 <- as.data.frame(p3)
    p4 <- df[Season == Season_prediction & Game_Week_Index == i,
                                            Game_Week_Index]
    p4 <- as.data.frame(p4)

    # now we are back to stitching our prediction table together
    AggP <- cbind(div1, p1, p2, p3, p4, P_Draw, P_Opposition, P_Team)
    colnames(AggP) <- c("League", "Team", "Season", "Opposition",
        "Game_Week_Index", "Euro_T_P_Draw", "Euro_T_P_Opposition",
        "Euro_T_P_Team")

    # save the results
    PredResults <- rbindlist(list(PredResults, AggP))
    print(i)
}

#--------------------- Export and merge to the calc data ---------------------#
# read in the existing calc data
setwd(paste0("C:/Users/ciana/OneDrive/SONY_16M1/Football Predictions/",
"Europe/Output Data"))
# Here we load in the output file for the model parameter
Calc_df <- read.csv("Europe Calc Data.csv", header = TRUE)
Calc_df <- setDT(Calc_df)
# merge our results
Calc_df <- merge(Calc_df, PredResults, by = c('Season', 'Team',
                'Opposition', 'Game_Week_Index'), all.x = T)
# Calculate actual vs predicted metrics
Calc_df[, Euro_T_Cl_Pred_Outcome := 0]
Calc_df[Euro_T_P_Team > Euro_T_P_Opposition & Euro_T_P_Team > Euro_T_P_Draw,
                                Euro_T_Cl_Pred_Outcome := 1]
Calc_df[Euro_T_P_Opposition > Euro_T_P_Team & Euro_T_P_Opposition > Euro_T_P_Draw,
                                Euro_T_Cl_Pred_Outcome := -1]
Calc_df[, Euro_T_Cl_Same := 0]
Calc_df[Actual_Outcome == Euro_T_Cl_Pred_Outcome, Euro_T_Cl_Same := 1]
Calc_df[, Euro_T_Cl_Pred_Winning_Odds := 0]
Calc_df[Euro_T_Cl_Pred_Outcome == -1, Euro_T_Cl_Pred_Winning_Odds
                                            := Opposition_Odds]
Calc_df[Euro_T_Cl_Pred_Outcome == 1, Euro_T_Cl_Pred_Winning_Odds := Team_Odds]
Calc_df[Euro_T_Cl_Pred_Outcome == 0, Euro_T_Cl_Pred_Winning_Odds := Draw_Odds]
Calc_df[, Bets := 200]
Calc_df[, Euro_T_Cl_Winnings := Bets * Euro_T_Cl_Pred_Winning_Odds * Euro_T_Cl_Same]

# Send it out to play in the traffic
write.csv(Calc_df, Pot, row.names = FALSE)

#--------------------------- Summary of results Log --------------------------#
# A Brief History of Time:
Model_time <- paste0('The model was run at ', lubridate::now())
# Overall accuracy for the season:
Calc_df <- Calc_df[complete.cases(Calc_df),]
Accuracy <- setDT(Calc_df[Season == Season_prediction, j = list(Cor_Pred =
mean(Euro_T_Cl_Same))])
Acc_Statement <- paste0('The accuracy is ', Accuracy)
# type of model run
Model_Type <- 'This is a: European Classification XGBoost'
# Any Notes
Notes <- 'New tuning method fully embracing MLR'
# Straight Profitability
Profit <- setDT(Calc_df[Season == Season_prediction, j = list(
sum(Euro_T_Cl_Winnings))]) - setDT(Calc_df[Season == Season_prediction, j = list(
sum(Bets))])
Profit_Statement <- paste0('Profits are ', Profit)
# change variable storage so it's a prettier list
variables <- paste(variables, collapse = ",")
# Create a summary
Summary <- c(Model_time, Acc_Statement, Model_Type, Notes, variables,
    Profit_Statement)
Summary <- as.data.frame(Summary)
# Read in log
Results_Log <- read.csv('Results Log.csv')
# add on to the end
Results_Log <- rbind(Results_Log, Summary)
# and publish
write.csv(Results_Log, 'Results Log.csv', row.names = F)

#------------------------------------ fin ------------------------------------#
