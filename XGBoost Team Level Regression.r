#---- this file creates an XGBoost classification model for European data ----#

###############################################################################
#------------------------------Package Loading--------------------------------#
###############################################################################
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
require(vtreat)

###############################################################################
#-----------------------------Parameter Settings------------------------------#
###############################################################################
# what Season are we predicting? (Enter numeric)
Season_prediction <- 20152016
# Are we doing a single market or Europe wide?
# Take away any leagues you don't want included:
# Full List: League <- c('D1','E0', 'F1', 'SP1', 'I1')
League <- c('D1', 'E0', 'F1', 'SP1', 'I1')

# How many games in a season?
GWRange <- 38 #- 38 games in a season son

# type of model run
Model_Type <- 'This is a: Team Regression XGBoost'
# Any Notes
Notes <- 'most significant variables, all vars'

###############################################################################
#------------------------------- Data Loading --------------------------------#
###############################################################################

# which project folder we want to work in
setwd(paste0("C:/Users/ciana/OneDrive/SONY_16M1/Football Predictions/Europe/",
      "Output Data"))
df <- read.csv("Europe Prepped Output.csv", header = TRUE)
df <- as.data.table(df)
#df <- df[complete.cases(df),]
#-------------------------- Apply Seasonal Filters ---------------------------#
# convert to numeric
df$Season <- gsub(" ", "", df$Season)
df$Season <- as.numeric(df$Season)
df <- df[Season <= Season_prediction,]

#---------------------------- Apply Market Filters ---------------------------#
df <- df[Div %in% League]

#----------------------------- Apply Team Filters ----------------------------#
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

###############################################################################
#--------------------------------Model Building-------------------------------#
###############################################################################

# ok so this is the meat of the action where for every league we...
for(j in 3:length(League)){

#------------------ Loop through every team and gameweek ---------------------#
    dt <- df[Div %in% League[j]]
    League_Teams <- unique(dt[Season == max(dt$Season),HomeTeam])
    dt$Team <- as.character(dt$Team)

	for(k in 1:length(League_Teams)){

		 for (i in 8:max(dt$Game_Week_Index)){
			 
#----------------- Define and transform model training set -------------------#

            ModTrain1 <- dt[Season < Season_prediction &
                            Team == League_Teams[k],]
			ModTrain2 <- dt[Season == Season_prediction &
                            Team == League_Teams[k]  & Game_Week_Index < i,]
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
            ModTrain <- as.data.frame(ModTrain)
            # Define the variables to be used and then create numeric dummies
            variables <- c('Season', 'Team_Favourite', 'Relative_Form',
            'Opposition_Shots_Conceded_Form', 'Team_Goals_Scored_Form',
            'Opposition_Goals_Scored_Form', 'Home_Away', 'Match_Tier',
            'Team_Goals_Conceded_Form', 'Opposition_Goals_Conceded_Form',
            'Team_Odds', 'Opposition_Odds', 'Poisson_Result', 'Regress_Result',
            'Team_Shots_Conceded_Form','Team_Handicap', 'Relative_Odds')
            TDat1 <- ModTrain[, variables]
            TDat2 <- dummyVars("~.", data = TDat1)
            TrainDat <- data.frame(predict(TDat2, newdata = TDat1))
            #- now we need to normalize the training data
            TrainDatnames <- colnames(TrainDat)
            #TrainDat <- normalizeData(TrainDat, type="0_1")
            colnames(TrainDat) <- TrainDatnames

            # Get the mean encoded team variables
            TDat3 <- ModTrain
            Dependent <- data.frame(ModTrain$Team_Goal_Diff)
            TDat3 <- cbind(TDat3, Dependent)
            colnames(TDat3)[ncol(TDat3)] <- "Dependent"
            Team_variables <- c('Opposition')
            cfe <- vtreat::mkCrossFrameNExperiment(TDat3, Team_variables,
                                            "Dependent")
            plan <- cfe$treatments
            Team_Vars <- cfe$crossFrame
            codes <- c('lev', 'catN', 'clean', 'isBAD')
            Team_Vars <- Team_Vars[, grep(paste(codes, collapse = "|"),
                                    colnames(Team_Vars), value = TRUE)]

            # Add in the mean encoded variables
            TrainDat <- cbind(TrainDat, Team_Vars)

            # same for the Dependent variable
            Dependent <- data.frame(ModTrain$Team_Goal_Diff)
            # for this package it wants a single column with the the three 
            # groups to classify
            TrainDat <- cbind(TrainDat, Dependent)

            # ok so to start modelling we have to declare a so called 'task', 
            # with the dependent and independent variables called out
            TrainDat <- as.data.frame(TrainDat)
            TrainDat[is.na(TrainDat)] <- 0
            trainTask <- makeRegrTask(data = TrainDat,
                            target = "ModTrain.Team_Goal_Diff")

            Agg_Results <- data.frame()

#---------------- Gradient Boosting (Create prediction data) -----------------#
            # Create the prediction dataset
             PredData <- dt[Team == League_Teams[k] & Game_Week_Index == i &
                            Season == Season_prediction,]
            # not every team play in every game week so if there's nothing to 
            # predict then there's no point building a model, so in that case
            # we ignore everything below and move on to the next week

            # oho there's more, so it turns out that there is a chance that
            # you can have played teams every year of your involvement that
            # get scraped out, leaving you with just one line, so we check:

             CheckData <- dt[Team == League_Teams[k] & Game_Week_Index == i
                            & Season != Season_prediction,]

            if (nrow(PredData) > 0 & nrow(CheckData) > 1) {

            # The below restates PredData because the dummy process throws a 
            # stupid error when there's only one line. And that's not my 
            # opinion, it's objectively stupid.
            PredData <- dt[Team == League_Teams[k] & Game_Week_Index == i,]

			# add in some juicy details
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
             Pred_Team_Vars <- tryCatch({ vtreat::prepare(plan,
                                        PredData, pruneSig = NULL)
            },
            error = function(Pred_Team_Vars)
                { Pred_Team_Vars <<- data.frame()
                return(Pred_Team_Vars)
            })
             if (length(Pred_Team_Vars) > 0) {
                 PredDat <- cbind(PredDat, Pred_Team_Vars)
             } else {
                 PredDat <- PredDat
             }
            

            # Now I have to dynamically filter to make up for that R mistake
            # Just to be clear: R's mistake. Definitely not mine
            Season_Col <- paste0("Season.", Season_prediction)
            Cal_Season_Col <- as.character(unique(dt[
            Season == Season_prediction & Game_Week_Index == i,
            Calendar_Season]))
            PredDat <- PredDat[PredDat[, eval(Season_Col)] == 1,]

            
			# apply the same treatments to the pred data as has been done to 
            # the training data
			
			PredDat <- as.matrix(PredDat)
			PredDat <- xgboost::xgb.DMatrix(PredDat)


#---------------- Gradient Boosting (Hyperparameter Tuning) ------------------#

			# When doing Hyperparameter tuning we need to save the model in a
			# local folder so we temporarily move to the below
			
			setwd ("C:/Users/ciana/Documents/Football Predictions/Europe")

			parallelStartSocket(3)
			ptm <- proc.time()
			ps = makeParamSet(
			makeIntegerParam("nrounds", lower = 1, upper = 30),
			makeIntegerParam("max_depth", lower = 3, upper = 10),
			makeNumericParam("lambda", lower=0.55, upper=0.60),
			makeNumericParam("eta", lower = 0.001, upper = 0.1),
			makeNumericParam("subsample", lower = 0.1, upper = 0.8),
			makeNumericParam("min_child_weight", lower = 0.5, upper = 8),
			makeNumericParam("colsample_bytree", lower = 0.2, upper = 0.8),
			makeDiscreteParam(id = "objective", values = c("reg:linear"),
			tunable = F))
			ctrl = makeTuneControlMBO()
			inner = makeResampleDesc("Subsample", iters = 3)
			# Tuning in Inner resampling loop
			lrn = makeTuneWrapper("regr.xgboost", resampling = inner, par.set
										= ps,control = ctrl, show.info = FALSE)

			# tuning in Outer resampling loop
			outer = makeResampleDesc("CV", iters = 3)
			r = resample(lrn, trainTask, resampling = outer, extract
											= getTuneResult,show.info = FALSE)
			parallelStop()
			proc.time()-ptm
			# Bring it back
            setwd(paste0("C:/Users/ciana/OneDrive/SONY_16M1/",
            "Football Predictions/Europe/Output Data"))

#--------------- Gradient Boosting (select best parameters) ------------------#

            # this is a little messy but we summarize the optimal fits
			for (p in 1:length(r$extract)){
			    a <- unlist(r$extract[[p]])
			    nrounds <- a$x.nrounds
			    max_depth <- a$x.max_depth
			    lambda <- a$x.lambda
			    Score <- a$y.mse.test.mean
			    eta <- a$x.eta
			    subsample <- a$x.subsample
			    min_child_weight <- a$x.min_child_weight
			    colsample_bytree <- a$x.colsample_bytree
			    objective <- "reg:linear"
				Classifier_Results <- as.data.frame(cbind(nrounds, max_depth,
                lambda, eta, Score, subsample, min_child_weight,
                colsample_bytree, objective))
				Agg_Results <- rbindlist(list(Agg_Results,Classifier_Results),
				fill=T)
				}

			# tidy up any messy bits
			Agg_Results[is.na(Agg_Results)] <- 0

			# find the most accurate row
			Agg_Results$Score <- as.numeric(as.character(Agg_Results$Score))
			Model_Structure <- Agg_Results[which.min(Agg_Results$Score),]
			# Transform Traindat back to being just numeric for 
            # the actual model build
            TrainDat <- TrainDat[,-ncol(TrainDat)]

#---------------- Gradient Boosting (build best model type) ------------------#

            PM_nrounds <- as.numeric(as.character(Model_Structure$nrounds))
			PM_max_depth <- as.numeric(as.character(Model_Structure$max_depth))
			PM_lambda <- as.numeric(as.character(Model_Structure$lambda))
			PM_eta <- as.numeric(as.character(Model_Structure$eta))
			PM_subsample <- as.numeric(as.character(Model_Structure$subsample))
            PM_min_child_weight <- as.numeric(as.character(
                                    Model_Structure$min_child_weight))
            PM_colsample_bytree <- as.numeric(as.character(
                                    Model_Structure$colsample_bytree))
			PM_objective <- as.character(Model_Structure$objective)

			TrainDat <- as.matrix(TrainDat)
			Dependent <- as.matrix(Dependent)
			dTrain <- xgboost::xgb.DMatrix(TrainDat, label = Dependent)
            param <- list(max_depth = PM_max_depth, lambda = PM_lambda,
            eta = PM_eta, subsample = PM_subsample,
            min_child_weight = PM_min_child_weight,
            colsample_bytree = PM_colsample_bytree)

            Pmod <- xgboost::xgb.train(param, dTrain, , nrounds = PM_nrounds,
								objective = PM_objective)

#---------------- Gradient Boosting (Find optimal c-values) ------------------#

            # Fit is a matrix of predicted values for each principle component
			# Act is the actual result in terms of goal difference
			iter <- which.min(Agg_Results$Score)
			Resers <- as.data.frame(r$pred)
			Resers <- Resers[Resers$iter == iter,]
			Resers <- Resers[,2:3]
			colnames(Resers)[1] <- "Act"
			Resers <- as.data.frame(Resers)

			# now create the cartesian list of c-value possibilities
			x <- c(0.1,0.25,0.5,0.75,1)
			CVala <- rep(x,each=length(x))
			CVala <- as.data.frame(CVala)
			CValb <- rep(x,times=length(x))
			CValb <- as.data.frame(CValb)
			CValues <- cbind(CVala,CValb)
			CValues <- as.data.frame(CValues)

			# now we run a for loop which goes through the C-value combinations
            # and checks what the story is with the old accuracy levels, for 
            # each combination we want to figure out what the maximum accuracy 
            # achieved is and then we want to see where the elbow point is for 
            # the pc's we'll get that by looking at where there's the greatest 
            # increase in predictions and take the first one
			a <- nrow(Resers)
			b <- ncol(Resers)

			# Create ResP1 which is the actual goal difference transformed in
            # to class results (-1,0,1) first column of our new dataframe will 
            # be the actual result
            ResP1 <- data.frame()
			# just create this as a temporary measure so that things are the 
            # right size
            ResP1 <- rep(1,nrow(Resers))
			ResP1 <- as.data.frame(ResP1)
			# classify the actual goal difference into the results
			ResP1$Act <- ifelse(Resers$Act >0,1,ifelse(Resers$Act < 0, -1, 0))
			# get rid of the temp column cause things are cool now size wise
			ResP1 <- ResP1[,-1]
			head(ResP1)
			ResP1 <- as.data.frame(ResP1)

			# ok shtuff gets a bit mad here so pay attention:
			# so for each of the possible C-value combinations we want to check what the
			# accuracy is ToSt is to house the
			ToSt <- as.data.frame(1)
			for (d in 1:nrow(CValues)){

				for(f in 2:ncol(Resers)){
					# for each principle component we classify the predicted
					# values in to what the result would be
					ResP1[,f] <- ifelse(Resers[,f] > CValues[d,1],1,
									ifelse(Resers[,f] < -1*CValues[d,2], -1, 0))
					}
				# Sust houses the count of correct predictions
				# first we set it to be a series of 1's so it's the right size,
				# as above
				SuSt <- rep(1,a)
				SuSt <- as.data.frame(SuSt)
				for (w in 2:ncol(ResP1)){
					# but now what we want to do is create a "truth" matrix 
                    # where 1 indicates a correct prediction
					SuSt[,w-1] <- ifelse(ResP1[,w] == ResP1[,1],1,0)
					}
				TempSt <- as.data.frame(1)
				# TempSt houses the accuracy of each principle component
				for (l in 1:ncol(SuSt)){
					# aggregate each of the columns of SuSt
					TempSt[,l] <- sum(SuSt[,l])
					# express it as a percentage of accuracy
					TempSt[2,l] <- TempSt[1,l] / nrow(SuSt)
					}
				# this is the max accuracy level
				M1 <- max(TempSt[2,])
				TempStp1 <- as.data.frame(1)
				# TempSp1 tells us the where the elbow points are for the 
                # principle components
                #for (m in 2:ncol(TempSt)){
				#	TempStp1[,m-1] <- TempSt[1,m] - TempSt[1,m-1]
				#	}

				# it's plus one because the first column is ignored because it 
                # can't be the elbow
				M2 <- match(max(TempStp1),TempStp1)+1
				# what's the highest accuracy achieved by the principle components
				M3 <- match(M1, TempSt[2,])
				# set up TempStp2 as a house for the summary information
				TempStp2 <- as.data.frame(1)
				TempStp2[,1] <- ModTrain$Team[1] # the Team we're modelling
				TempStp2[,2] <- CValues[d,1] # the positive C-Value
				TempStp2[,3] <- CValues[d,2] # the negative C-Value
				TempStp2[,4] <- M1 # the accuracy
				TempStp2[,5] <- M2 # the Elbow P.C
				TempStp2[,6] <- M3 # the maximum accuracy

			# then if this is the first set of C-Values we're running through 
            # save ToSt as TempStp2 else tack it on at the end
				if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
				}


			}
		# rename the matrix that has the different results based on all the 
        # c - values
        colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", 
		 						"Max Accuracy", "Elbow P.C", "Max Acc P.C")

		# this is the row where the maximum accuracy is across the c-values
		PC1 <- match(max(ToSt[,4]),ToSt[,4])
		# ToStp is just going to be that row
		ToStp <- ToSt[PC1,]
		# this below bit is to make sure that if a team appears more than once in a
		# gameweek we pick up all matches
		ap <- nrow(PredData)

		# now we are back to stitching our prediction table together
		PredData$Season <- as.numeric(PredData$Season)
		PredData <- as.data.table(PredData)

		div1 <- dt[Season == Season_prediction & Game_Week_Index == i
												& Team == League_Teams[k], Div]
		div1 <- as.data.frame(div1)
		p1 <- dt[Season == Season_prediction & Game_Week_Index == i
												& Team == League_Teams[k],Team]
		p1 <- as.data.frame(p1)
		p2 <- rep(Season_prediction,nrow(PredDat))
		p2 <- as.data.frame(p2)
		p3 <- dt[Season == Season_prediction & Game_Week_Index == i
										& Team == League_Teams[k],Opposition]
		p3 <- as.data.frame(p3)
		p4 <- dt[Season == Season_prediction & Game_Week_Index == i
									& Team == League_Teams[k], Game_Week_Index]
		p4 <- as.data.frame(p4)
		p5a <- predict(Pmod, PredDat)
		p5a <- as.data.frame(p5a)
		AggP <- cbind(p1,p2,p3,p4,p5a,ToStp[,2],ToStp[,3],ToStp[,4]) # stitched together
		colnames(AggP) <- c("Team", "Season", "Opposition", "Game_Week_Index",
                    "Team_Prediction", "Team_Pos_C_Value", "Team_Neg_C_Value",
                "Team_Max_Accuracy")
		# save the results
			PredResults <- rbindlist(list(PredResults,AggP))
		# let a worrying parent know what progress their child is making
		print(j)
		print(k)
		print(i)
		} # end if PredData > 0
		} # end for loop on GW's
	} # end for loop on Team
} # end for loop on League

#-------------------- Export and merge to the calc data ----------------------#
# read in the existing calc data
setwd(paste0("C:/Users/ciana/OneDrive/SONY_16M1/Football Predictions/",
    "Europe/Output Data"))
Calc_df <- read.csv("Europe Calc Data Output.csv", header = TRUE)
Calc_df <- setDT(Calc_df)
# merge our results
Calc_df <- merge(Calc_df, PredResults, by = c('Season', 'Team', 'Opposition',
                            'Game_Week_Index'), all.x = T)
# Calculate actual vs predicted metrics
Calc_df[, Team_Pred_Outcome := 0]
Calc_df[Team_Prediction >= Team_Pos_C_Value, Team_Pred_Outcome := 1]
Calc_df[Team_Prediction <= Team_Neg_C_Value * -1, Team_Pred_Outcome := -1]
Calc_df[, Team_Same := 0]
Calc_df[Actual_Outcome == Team_Pred_Outcome, Team_Same := 1]
Calc_df[, Team_Pred_Winning_Odds := 0]
Calc_df[Team_Pred_Outcome == -1, Team_Pred_Winning_Odds := Opposition_Odds]
Calc_df[Team_Pred_Outcome == 1, Team_Pred_Winning_Odds := Team_Odds]
Calc_df[Team_Pred_Outcome == 0, Team_Pred_Winning_Odds := Draw_Odds]
Calc_df[, Team_Winnings := Bets * Team_Pred_Winning_Odds * Team_Same]

# Send it out to play in the traffic
write.csv(Calc_df, "Europe Calc Data Output.csv", row.names = FALSE)

#--------------------------- Summary of results Log --------------------------#
# A Brief History of Time:
Model_time <- paste0('The model was run at ', lubridate::now())
# Overall accuracy for the season:
Calc_df <- Calc_df[complete.cases(Calc_df),]
Accuracy <- setDT(Calc_df[Season == Season_prediction, j = list(Cor_Pred =
  mean(League_Same))])
Acc_Statement <- paste0('The accuracy is ', Accuracy)

# Straight Profitability
Profit <- setDT(Calc_df[Season == Season_prediction, j = list(
sum(Team_Winnings))]) - setDT(Calc_df[Season == Season_prediction, j = list(
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
