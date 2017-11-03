#-- this file creates an XGBoost classification model for European data --#

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
require(xgboost)

##############################################################################
#-----------------------------Parameter Settings-----------------------------#
##############################################################################
# what Season are we predicting? (Enter numeric)
Season_prediction <- 20152016
# Are we doing a single market or Europe wide?
# Take away any leagues you don't want included:
# Full List: League <- c('D1','E0', 'F1', 'SP1')
League <- c('E0', 'F1', 'SP1')

# How many games in a season?
GWRange <- 38 #- 38 games in a season son

##############################################################################
#--------------------------------Data Loading--------------------------------#
##############################################################################

# which project folder we want to work in
setwd ("C:/Users/coloughlin/OneDrive/SONY_16M1/Football Predictions/Europe/Output Data")
df <- read.csv("Europe Prepped Output.csv", header = TRUE)
df <- as.data.table(df)

# TODO Apply Season filters
# convert to numeric
df$Season <- gsub(" ", "", df$Season)
df$Season <- as.numeric(df$Season)
df <- df[Season <= Season_prediction,]

# TODO Apply Market filters
df <- df[Div %in% League]

# TODO Apply Team filtering
# Teams in current season
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

# Create empty containers to hold our results later:
PredResults <- data.frame()
StatResults <- data.frame()

##############################################################################
#-------------------------------Model Building-------------------------------#
##############################################################################

#- ok so this is the meat of the action where for every team we...
for(j in 1: 2){
		 #- and for every gameweek
		 for (i in 8:25){
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6
			ModTrain1 <- train[train$Game.Week.Index > 6 & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Game.Week.Index > 6 & train$Game.Week.Index < i & train$Season == "2015 2016",]
			ModTrain <- rbind(ModTrain1,ModTrain2)

			#-we'll create a couple of extra variables to make things a little easier for the model here each time
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- (ModTrain$Opposition.Shots.on.Target.Form + ModTrain$Team.Shots.Conceded.Form)
			ModTrain$Relative.Form <- ModTrain$Team.Form - ModTrain$Opposition.Form
			ModTrain$Expected.Shots <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form) - (ModTrain$Team.Shots.Conceded.Form + ModTrain$Opposition.Shots.on.Target.Form)
			ModTrain$Relative.Odds <- ModTrain$Opposition.Odds - ModTrain$Team.Odds
			#ModTrain$Team_Tier <- ifelse(ModTrain$Team_Tier == 1, "Tier 1", ifelse(ModTrain$Team_Tier == 2, "Tier 2",ifelse(ModTrain$Team_Tier == 3, "Tier 3", "Tier 4" ) ))
			#ModTrain$Opposition_Tier <- ifelse(ModTrain$Opposition_Tier == 1, "Tier 1", ifelse(ModTrain$Opposition_Tier == 2, "Tier 2",ifelse(ModTrain$Opposition_Tier == 3, "Tier 3", "Tier 4" ) ))

			for (q in 1 : nrow(ModTrain)){
			ModTrain$Expected.Goal.Difference[q] <- ((ave(rpois(50,ModTrain$Expected.Team.Goals[[q]]))[1] + ModTrain$Team.Handicap[[q]]) - (ave(rpois(50,ModTrain$Expected.Opposition.Goals[[q]]))[1]))
			}
			#- here we play a little with the dependent, kind of depends on the model we're running,in this case we want a trinomial classification
			#ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff > 0, 2, ifelse(ModTrain$Team.Goal.Diff < 0, 0, 1))
			#ModTrain$Team.Goal.Diff <- as.factor(ModTrain$Team.Goal.Diff)


			#- we have to feed it the input dataset of all the variables used, so define the formula below
			variables <- c("Season","Calendar_Season","Team_Tier","Opposition_Tier","Div","Team.Favourite","Expected.Goal.Difference","Team.Form","Opposition.Form","Team.Shots.on.Target.Form",
			"Opposition.Shots.Conceded.Form","Opposition.Shots.on.Target.Form","Team.Shots.Conceded.Form","Relative.Goals.Form","Home.Away",
			"Team.Odds","Draw.Odds","Streak.Probability","Asian.Handicap","Opposition.Odds","Relative.Odds","Expected.Shots","Relative.Form")
			TDat1 <- ModTrain[,variables]
			#- but on top of that you can't have categorical variables within the X matrix so to speak
			#- so we use a function to turn our categorical stuff into numeric data
			TDat2 <- dummyVars("~.",data=TDat1)
			TrainDat <- data.frame(predict(TDat2, newdata = TDat1))
			#- now we need to normalize the training data
			TrainDatnames <- colnames(TrainDat)
			#TrainDat <- normalizeData(TrainDat, type="0_1")
			colnames(TrainDat) <- TrainDatnames

			#- same for Test data
			Tester <- data.frame(ModTrain$Team.Goal.Diff)
			#TDat2 <- dummyVars("~.",data=Tester)
			#TestDat <- data.frame(predict(TDat2, newdata = Tester))
			#- now we need to normalize the training data
			#TestDatnames <- colnames(TestDat)
			#TestDat <- normalizeData(TestDat, type="0_1")
			#colnames(TestDat) <- TestDatnames

            # for this package it wants a single column with the the three groups to classify
            # so join this to TrainDat and use instead of TestDat
            TrainDat <- cbind(TrainDat, Tester)

            # ok so to start modelling we have to declare a so called 'task', with the
            # dependent and independent variables called out
			TrainDat <- as.data.frame(TrainDat)
            trainTask <- makeRegrTask(data = TrainDat,target = "ModTrain.Team.Goal.Diff")

            Agg_Results <- data.frame()

            #------------------------------ Gradient Boosting ------------------------------# 6 minutes
            ## Tuning in inner resampling loop

	            parallelStartSocket(3)

	            ptm <- proc.time()
	            ps = makeParamSet(
	              makeIntegerParam("nrounds", lower = 1, upper = 30),
	              makeIntegerParam("max_depth", lower = 3, upper = 20),
	              makeNumericParam("lambda", lower=0.55, upper=0.60),
	              makeNumericParam("eta", lower = 0.001, upper = 0.5),
	              makeNumericParam("subsample", lower = 0.1, upper = 0.8),
	              makeNumericParam("min_child_weight", lower = 1, upper = 5),
				  makeNumericParam("colsample_bytree", lower = 0.2, upper = 0.8)
	              #makeDiscreteParam(id = "objective", values = c("reg:linear"), tunable = F)
	            )
	            ctrl = makeTuneControlMBO()
	            inner = makeResampleDesc("Subsample", iters = 2)
	            lrn = makeTuneWrapper("regr.xgboost", resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

	            ## Outer resampling loop
	            outer = makeResampleDesc("CV", iters = 3)
	            r = resample(lrn, trainTask, resampling = outer, extract = getTuneResult, show.info = FALSE)
	            parallelStop()
	            proc.time()-ptm

				## pick out the best performing model type (not normally recommended)
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
				Classifier_Results <- as.data.frame(cbind(nrounds, max_depth, lambda, eta, Score, subsample, min_child_weight, colsample_bytree, objective))
				Agg_Results <- rbindlist(list(Agg_Results,Classifier_Results), fill=T)
				}

				Agg_Results <- as.data.frame(Agg_Results)
				for(l in c(1:8)){
				    d <- lapply(Agg_Results[,l], as.character, stringsAsFactors=FALSE)
				    d <- unlist(d)
				    d <- as.data.frame(d)
				    d[,1] <- as.character(d[,1])
				    d[,1] <- as.numeric(d[,1])
				    Agg_Results[,l] <- d
				}


				Agg_Results[is.na(Agg_Results)] <- 0

				# find the most accurate row
				Model_Structure <- Agg_Results[which.min(Agg_Results$Score),]
				# Transform Traindat back to being just numeric for the actual model build
				TrainDat <- TrainDat[,-ncol(TrainDat)]
				# Define the ideal model structure
				PM_nrounds <- as.numeric(as.character(Model_Structure$nrounds))
				PM_max_depth <- as.character(Model_Structure$max_depth)
				PM_lambda <- as.character(Model_Structure$lambda)
				PM_eta <- as.character(Model_Structure$eta)
				PM_subsample <- as.character(Model_Structure$subsample)
				PM_min_child_weight <- as.character(Model_Structure$min_child_weight)
				PM_colsample_bytree <- as.character(Model_Structure$colsample_bytree)
				PM_objective <- as.character(Model_Structure$objective)

				TrainDat <- cbind(TrainDat,Tester)
				TrainDat <- as.matrix(TrainDat)

				dTrain <- xgb.DMatrix(TrainDat, label = TrainDat[,ncol(TrainDat)])
				param <- list(max_depth = PM_max_depth, lambda = PM_lambda, eta = PM_eta, subsample = PM_subsample, min_child_weight = PM_min_child_weight,
								colsample_bytree = PM_colsample_bytree)
				Pmod <- xgb.train(param, dTrain, , nrounds = PM_nrounds)



			PredData <- train[train$Team == Teams[j,1] & train$Game.Week.Index == i & train$Season == "2015 2016",]

			#- If any of the extra variables we created in Modtrain are used in the model we need to create them in PredData as well,so that happens here
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			PredData$Relative.Form <- PredData$Team.Form - PredData$Opposition.Form
			PredData$Expected.Shots <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form) - (PredData$Team.Shots.Conceded.Form + PredData$Opposition.Shots.on.Target.Form)
			PredData$Relative.Odds <- PredData$Opposition.Odds - PredData$Team.Odds
			#PredData$Team_Tier <- ifelse(PredData$Team_Tier == 1, "Tier 1", ifelse(PredData$Team_Tier == 2, "Tier 2",ifelse(PredData$Team_Tier == 3, "Tier 3", "Tier 4" ) ))
			#PredData$Opposition_Tier <- ifelse(PredData$Opposition_Tier == 1, "Tier 1", ifelse(PredData$Opposition_Tier == 2, "Tier 2",ifelse(PredData$Opposition_Tier == 3, "Tier 3", "Tier 4" ) ))

			#-sometimes a team won't be playing in the subsequent gameweek so this database will be empty,so move on folks nothing to see here
			if(nrow(PredData) > 0){
			PredData$Expected.Team.Goals <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form)
			PredData$Expected.Opposition.Goals <- (PredData$Opposition.Shots.on.Target.Form + PredData$Team.Shots.Conceded.Form)
			for (q in 1 : nrow(PredData)){
			PredData$Expected.Goal.Difference[q] <- (ave(rpois(50,PredData$Expected.Team.Goals[[q]]))[1]  + PredData$Team.Handicap[[q]]) - ave(rpois(50,PredData$Expected.Opposition.Goals[[q]]))[1]
			}

			#PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff > 0, 2, ifelse(PredData$Team.Goal.Diff < 0, 0, 1))
			#PredData$Team.Goal.Diff <- as.factor(PredData$Team.Goal.Diff)


			if( PredData$Opposition %in% ModTrain$Opposition){
			#- we need to set up PredData to be in the same format as the Training and Test data
			PDat1 <- PredData[,variables]
			#- but on top of that you can't have categorical variables within the X matrix so to speak
			#- so we use a function to turn our categorical stuff into numeric data
			PDat2 <- dummyVars("~.",data=PDat1)
			PredDat <- data.frame(predict(PDat2, newdata = PDat1))
			#- now we need to normalize the training data
			PredDatnames <- colnames(PredDat)
			#PredDat <- normalizeData(PredDat, type="0_1")
			colnames(PredDat) <- PredDatnames



		#- Fit is a matrix of predicted values for each principle component
		#-Act is the actual result in terms of goal difference
		Fit <- predict(Pmod, TrainDat)
		Fit <- as.data.frame(Fit)
		Act <- Tester
		#- Resers siameses them together
		Resers <- cbind(Act,Fit)
		Resers <- as.data.frame(Resers)

		#-now create the cartesian list of c-value possibilities
		x <- c(0.1,0.25,0.5,0.75,1)
		CVala <- rep(x,each=length(x))
		CVala <- as.data.frame(CVala)
		CValb <- rep(x,times=length(x))
		CValb <- as.data.frame(CValb)
		CValues <- cbind(CVala,CValb)
		CValues <- as.data.frame(CValues)

		#- now we run a for loop which goes through the C-value combinations and checks what the story
		#- is with the old accuracy levels, for each combination we want to figure out what the maximum accuracy achieved is
		#- and then we want to see where the elbow point is for the pc's
		#- we'll get that by looking at where there's the greatest increase in predictions and take the first one

		a <- nrow(Resers)
		b <- ncol(Resers)

		#- Create ResP1 which is the actual goal difference transformed in to class results (-1,0,1)
		#-first column of our new dataframe will be the actual result
		ResP1 <- data.frame()
		ResP1 <- rep(1,nrow(Resers)) #- just create this as a temporary measure so that things are the right size
		ResP1 <- as.data.frame(ResP1)
		ResP1$Act <- ifelse(Resers[,1] >0,1,ifelse(Resers[,1] < 0, -1, 0)) #- classify the actual goal difference into the results
		ResP1 <- ResP1[,-1] #- get rid of the temp column cause things are cool now size wise
		head(ResP1)
		ResP1 <- as.data.frame(ResP1)


		#-ok shtuff gets a bit mad here so pay attention:
		#- so for each of the possible C-value combinations we want to check what the accuracy is
		#- ToSt is to house the
		ToSt <- as.data.frame(1)
		for (d in 1:nrow(CValues)){

		for(f in 2:ncol(Resers)){
		#- for each principle component we classify the predicted values in to what the result would be
		ResP1[,f] <- ifelse(Resers[,f] > CValues[d,1],1,ifelse(Resers[,f] < -1*CValues[d,2], -1, 0))
		}
			#- Sust houses the count of correct predictions
			#- first we set it to be a series of 1's so it's the right size, as above
			SuSt <- rep(1,a)
			SuSt <- as.data.frame(SuSt)
			for (k in 2:ncol(ResP1)){
			#- but now what we want to do is create a "truth" matrix where 1 indicates a correct prediction
			SuSt[,k-1] <- ifelse(ResP1[,k] == ResP1[,1],1,0)
			}
			TempSt <- as.data.frame(1)
				#-TempSt houses the accuracy of each principle component
				for (l in 1:ncol(SuSt)){
				TempSt[,l] <- sum(SuSt[,l]) #- aggregate each of the columns of SuSt
				TempSt[2,l] <- TempSt[1,l] / nrow(SuSt) #- express it as a percentage of accuracy
				}
					M1 <- max(TempSt[2,]) #- this is the max accuracy level
					TempStp1 <- as.data.frame(1)
					#-TempSp1 tells us the where the elbow points are for the principle components
					#for (m in 2:ncol(TempSt)){
					#TempStp1[,m-1] <- TempSt[1,m] - TempSt[1,m-1]
					#}

					#M2 <- match(max(TempStp1),TempStp1)+1 #- it's plus one because the first column is ignored because it can't be the elbow
					M3 <- match(M1, TempSt[2,]) #- what's the highest accuracy achieved by the principle components
					#- set up TempStp2 as a house for the summary information
					TempStp2 <- as.data.frame(1)
					TempStp2[,1] <- ModTrain[1,88] #- the Team we're modelling
					TempStp2[,2] <- CValues[d,1] #- the positive C-Value
					TempStp2[,3] <- CValues[d,2] #- the negative C-Value
					TempStp2[,4] <- M1 #- the accuracy
					#TempStp2[,5] <- M2 #- the Elbow P.C
					TempStp2[,5] <- M3 #- the maximum accuracy

					#-then if this is the first set of C-Values we're running through save ToSt as TempStp2 else tack it on at the end
					if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
					}


		}
		#-rename the matric that has the different results based on all the c-values
		#colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", "Max Accuracy", "Elbow P.C", "Max Acc P.C")

				PC1 <- match(max(ToSt[,4]),ToSt[,4]) #-this is the row where the maximum accuracy is across the c-values
		ToStp <- ToSt[PC1,] #-ToStp is just going to be that row
		#-this below bit is to make sure that if a team appears more than once in a gameweek we pick up all matches
		ap <- nrow(PredData)




	p1 <- PredData$Team
	p1 <- as.data.frame(p1)
	p2 <- "2015 2016"
	p3 <- PredData$Opposition
	p3 <- as.data.frame(p3)
	p4 <- PredData$Game.Week.Index
	p4 <- as.data.frame(p4)

	PredDat <- as.matrix(PredDat)
	PredDat <- xgb.DMatrix(PredDat)
	#-now we are back to stitching our prediction table together
	p5a <- predict(Pmod, PredDat) #- this gives a prediction for each principle component
	p5a <- as.data.frame(p5a)
	p5b <- ncol(p5a)
	p5 <- p5a[,ToStp[1,5]] #- this select the elbow pc from the prediction matrix
	p5 <- as.data.frame(p5)
	AggP <- cbind(p1,p2,p3,p4,p5,ToStp[,2],ToStp[,3],ToStp[,4]) #- stitched together
	colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "Prediction", "Pos C-Value", "Neg C-Value", "Max Accuracy")

	#- save the results
PredResults <- rbindlist(list(PredResults,AggP))

	}
	}
}
}



write.csv(PredResults, "Lily's Prediction 2015 2016 France.csv")
