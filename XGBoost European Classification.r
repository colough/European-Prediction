#---- this file creates an XGBoost classification model for European data -----#

##############################################################################
#------------------------------Package Loading-------------------------------#
##############################################################################
# You wouldn't go shopping without your wallet, don't forget your packages
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

##############################################################################
#-----------------------------Parameter Settings-----------------------------#
##############################################################################
# what Season are we predicting? (Enter numeric)
Season_prediction <- 20152016
# Are we doing a single market or Europe wide?
# Take away any leagues you don't want included:
# Full List: League <- c('D1','E0', 'F1', 'SP1')
League <- c('D1', 'E0', 'F1', 'SP1')

# How many games in a season?
GWRange <- 38 #- 38 games in a season son

##############################################################################
#--------------------------------Data Loading--------------------------------#
##############################################################################

# which project folder we want to work in
#setwd ("C:/Users/coloughlin/OneDrive/SONY_16M1/Football Predictions/Europe/Output Data")
setwd ("C:/Users/ciana/OneDrive/SONY_16M1/Football Predictions/Europe/Output Data")
df <- read.csv("Europe Prepped Output.csv", header = TRUE)
df <- as.data.table(df)
df <- df[complete.cases(df),]
#--------------------------- Apply Seasonal Filters ---------------------------#
# convert to numeric
df$Season <- gsub(" ", "", df$Season)
df$Season <- as.numeric(df$Season)
df <- df[Season <= Season_prediction,]

#---------------------------- Apply Market Filters ----------------------------#
df <- df[Div %in% League]

#----------------------------- Apply Team Filters -----------------------------#
# Only want to build models for teams who are in current season
Teams <- unique(df[Season == max(df$Season),HomeTeam])
# Can only take teams whose first season isn't the one currently predicting:
# So create unique list of Seasons and teams and then look at all teams with
# greater than one row
df$HomeTeam <- as.character(df$HomeTeam)
Temp_Teams <- as.data.frame(unique(df[HomeTeam %in% Teams,c("HomeTeam",
													"Season")]))
Temp_Teams <- as.data.table(Temp_Teams)
Team_Count <- Temp_Teams[,.N, by=HomeTeam]
Team_Count <- Team_Count[N > 1]
Teams <- Team_Count[,c("HomeTeam")]
df <- df[df$HomeTeam %in% Teams$HomeTeam,]
df <- df[df$AwayTeam %in% Teams$HomeTeam,]

# Create empty containers to hold our results later:
PredResults <- data.frame()
StatResults <- data.frame()

##############################################################################
#-------------------------------Model Building-------------------------------#
##############################################################################

# ok so this is the meat of the action where for every team we...
# for(j in 1: 2){
#------------------------ Loop through every gameweek -------------------------#
 for (i in 8:GWRange){
	 #i=17
#------------------ Define and transform model training set -------------------#
	ModTrain1 <- df[Season < Season_prediction,]
	ModTrain2 <- df[Season == Season_prediction & Game_Week_Index < i,]
	ModTrain <- rbindlist(list(ModTrain1,ModTrain2))
	ModTrain <- ModTrain[Game_Week_Index >= 7,]
	# we'll create a couple of extra variables to make things a little
	# easier for the model here each time
	ModTrain$Relative_Form <- ModTrain$Team_Form -
								ModTrain$Opposition_Form
	ModTrain$Relative_Odds <- ModTrain$Opposition_Odds -
								ModTrain$Team_Odds
	# Turn Seasons back into a factor so we can use it in the model
	ModTrain$Season <- as.factor(ModTrain$Season)
	# As we're doing a classification model we want the dependent to be
	# classes
	ModTrain$Team_Goal_Diff <- ifelse(ModTrain$Team_Goal_Diff > 0, 2,
							ifelse(ModTrain$Team_Goal_Diff < 0, 0, 1))
	ModTrain$Team_Goal_Diff <- as.factor(ModTrain$Team_Goal_Diff)
	ModTrain <- as.data.frame(ModTrain)
	# Define the variables to be used and then create numeric dummies
	variables <- c('Season','Div','Calendar_Season','Match_Tier','Home_Away',
	'Poisson_Result','Regress_Result','Relative_Form','Relative_Goals_Conceded_Form',
	'Team_Handicap','Relative_Odds','Regress_Home','Regress_Away','Poisson_Home_Win',
	'Poisson_Away_Win','Team_Favourite','Team_Odds','Team','Opposition')
	TDat1 <- ModTrain[,variables]
	TDat2 <- dummyVars("~.",data=TDat1)
	TrainDat <- data.frame(predict(TDat2, newdata = TDat1))
	#- now we need to normalize the training data
	TrainDatnames <- colnames(TrainDat)
	#TrainDat <- normalizeData(TrainDat, type="0_1")
	colnames(TrainDat) <- TrainDatnames

	# same for the Dependent variable
	Dependent <- data.frame(ModTrain$Team_Goal_Diff)
  	# for this package it wants a single column with the the three groups
		# to classify
 	TrainDat <- cbind(TrainDat, Dependent)

	# ok so to start modelling we have to declare a so called 'task', with the
	# dependent and independent variables called out
	TrainDat <- as.data.frame(TrainDat)
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


	PDat1 <- PredData[,variables]
	PDat2 <- dummyVars("~.",data=PDat1)
	PredDat <- data.frame(predict(PDat2, newdata = PDat1))
	#- now we need to normalize the prediction data
	PredDatnames <- colnames(PredDat)
	#PredDat <- normalizeData(PredDat, type="0_1")
	colnames(PredDat) <- PredDatnames

	# Now I have to dynamically filter to make up for that R mistake
	# Just to be clear: R's mistake. Definitely not mine
	Season_Col <- paste0("Season.",Season_prediction)
	Cal_Season_Col <- as.character(unique(df[Season == Season_prediction &
						Game_Week_Index == i,Calendar_Season]))
	PredDat <- PredDat[PredDat[,eval(Season_Col)] == 1, ]

	PredDat <- as.matrix(PredDat)
	PredDat <- xgboost::xgb.DMatrix(PredDat)


#----------------- Gradient Boosting (Hyperparameter Tuning) ------------------#

	# When doing Hyperparameter tuning we need to save the model in a local
	# folder so we temporarily move to the below
	#setwd ("C:/Users/coloughlin/Documents/Temp/Update/Football Predictions/Europe")
	setwd ("C:/Users/ciana/Documents/Football Predictions/Europe")

	parallelStartSocket(3)
	ptm <- proc.time()
	ps = makeParamSet(
	makeIntegerParam("nrounds", lower = 1, upper = 30),
	makeIntegerParam("max_depth", lower = 3, upper = 20),
	makeNumericParam("lambda", lower=0.55, upper=0.60),
	makeNumericParam("eta", lower = 0.001, upper = 0.5),
	makeNumericParam("subsample", lower = 0.1, upper = 0.8),
	makeNumericParam("min_child_weight", lower = 1, upper = 8),
	makeNumericParam("colsample_bytree", lower = 0.2, upper = 0.8),
	makeDiscreteParam(id = "objective", values = c("multi:softprob"), tunable = F)
	)
	ctrl = makeTuneControlMBO()
	inner = makeResampleDesc("Subsample", iters = 2)
	# Tuning in Inner resampling loop
	lrn = makeTuneWrapper("classif.xgboost", resampling = inner, par.set = ps,
								control = ctrl, show.info = FALSE)

	# tuning in Outer resampling loop
	outer = makeResampleDesc("CV", iters = 3)
	r = resample(lrn, trainTask, resampling = outer, extract = getTuneResult,
														show.info = FALSE)
	parallelStop()
	proc.time()-ptm
	# Bring it back
	#setwd ("C:/Users/coloughlin/OneDrive/SONY_16M1/Football Predictions/Europe/Output Data")
	setwd ("C:/Users/ciana/OneDrive/SONY_16M1/Football Predictions/Europe/Output Data")

#---------------- Gradient Boosting (select best parameters) ------------------#
	# this is a little messy but we summarize the optimal fits
	for (p in 1:length(r$extract)){
	    a <- unlist(r$extract[[p]])
	    nrounds <- a$x.nrounds
	    max_depth <- a$x.max_depth
	    lambda <- a$x.lambda
	    Score <- a$y.mmce.test.mean
	    eta <- a$x.eta
	    subsample <- a$x.subsample
	    min_child_weight <- a$x.min_child_weight
	    colsample_bytree <- a$x.colsample_bytree
	    objective <- "multi:softprob"
		Classifier_Results <- as.data.frame(cbind(nrounds, max_depth, lambda,
		eta, Score, subsample, min_child_weight, colsample_bytree, objective))
		Agg_Results <- rbindlist(list(Agg_Results,Classifier_Results), fill=T)
		}

	# tidy up any messy bits
	Agg_Results[is.na(Agg_Results)] <- 0

	# find the most accurate row
	Model_Structure <- Agg_Results[which.max(Agg_Results$Score),]
	# Transform Traindat back to being just numeric for the actual model build
	TrainDat <- TrainDat[,-ncol(TrainDat)]

#----------------- Gradient Boosting (build best model type) ------------------#
	PM_nrounds <- as.numeric(as.character(Model_Structure$nrounds))
	PM_max_depth <- as.character(Model_Structure$max_depth)
	PM_lambda <- as.character(Model_Structure$lambda)
	PM_eta <- as.character(Model_Structure$eta)
	PM_subsample <- as.character(Model_Structure$subsample)
	PM_min_child_weight <- as.character(Model_Structure$min_child_weight)
	PM_colsample_bytree <- as.character(Model_Structure$colsample_bytree)
	PM_objective <- as.character(Model_Structure$objective)

	TrainDat <- as.matrix(TrainDat)
	Dependent <- as.matrix(Dependent)
	dTrain <- xgboost::xgb.DMatrix(TrainDat, label = Dependent)
	param <- list(max_depth = PM_max_depth, lambda = PM_lambda, eta = PM_eta,
			subsample = PM_subsample, min_child_weight = PM_min_child_weight,
			colsample_bytree = PM_colsample_bytree)
	Pmod <- xgboost::xgb.train(param, dTrain, , nrounds = PM_nrounds,
						objective = PM_objective, num_class = 3)

#--------------- Gradient Boosting (Prediction & Context data) ----------------#

	#- Fit is a column of our predicted values
	#-Act is the actual result in terms of goal difference
	Fit <- predict(Pmod, PredDat)
	Fit <- as.data.table(Fit)
	# have to split up the predictions
	Fit_index <- as.data.frame(rep(c(0,1,2),nrow(Fit)/3))
	colnames(Fit_index) <- "Fit_Index"
	Fit <- cbind(Fit,Fit_index)
	P_Draw <- Fit[ Fit_Index == 1 , 1]
	P_Opposition <- Fit[ Fit_Index == 0 , 1]
	P_Team <- Fit[ Fit_Index == 2 , 1]

	PredData$Season <- as.numeric(PredData$Season)
	PredData <- as.data.table(PredData)

	div1 <- df[Season == Season_prediction & Game_Week_Index == i, Div]
	div1 <- as.data.frame(div1)
	p1 <- df[Season == Season_prediction & Game_Week_Index == i,Team]
	p1 <- as.data.frame(p1)
	p2 <- rep(Season_prediction,nrow(PredDat))
	p2 <- as.data.frame(p2)
	p3 <- df[Season == Season_prediction & Game_Week_Index == i,Opposition]
	p3 <- as.data.frame(p3)
	p4 <- df[Season == Season_prediction & Game_Week_Index == i,
													Game_Week_Index]
	p4 <- as.data.frame(p4)

	#-now we are back to stitching our prediction table together
	AggP <- cbind(div1,p1,p2,p3,p4,P_Draw,P_Opposition,P_Team) #- stitched together
	colnames(AggP) <- c("League","Team", "Season", "Opposition", "Game.Week.Index",
						"P-Draw","P-Opposition","P-Team")

	#- save the results
	PredResults <- rbindlist(list(PredResults,AggP))


	}



write.csv(PredResults, "Prediction 2015 2016.csv")
