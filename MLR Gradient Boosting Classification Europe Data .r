# this file tests a fully automated season prediction for one team in France
# this is a test update
#-- You wouldn't go shopping without your wallet and bags, don't forget your packages
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


#-which project folder we want to work in
#setwd ("C:/Users/coloughlin/Documents/Temp/Update/Football Predictions/Europe")
setwd ("C:/Users/ciana/Documents/Football Predictions/Europe")
DATA <- read.csv("Europe Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2015 2016",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)
TeamData <- data.frame


#- Then what we do is create the dataset so that instead of having the raw data and home and away teams we create a data set where
#- we have each team and their associated opposition for the game week, easier to see correlations that make sense that way
#- below we split it by home and away and then match up by team
for(i in 1:nrow(Teams)){
HData <- DATA[DATA$HomeTeam == Teams[i,1],]
#-Create the variables that we need - first for all the home matches
HData$Team.Favourite = HData$Home.Favourite
HData$Opposition = HData$AwayTeam
HData$Team.Form = HData$Home.Form
HData$Opposition.Form = HData$Away.form
HData$Team.Shots.on.Target.Form = HData$Home.Shots.on.Target.Form
HData$Opposition.Shots.on.Target.Form = HData$Away.Shots.on.Target.Form
HData$Team.Shots.Conceded.Form = HData$Home.Shots.Conceded.Form
HData$Opposition.Shots.Conceded.Form = HData$Away.Shots.Conceded.Form
HData$Team.Goals.Scored.Form = HData$Home.Goals.Scored.Form
HData$Opposition.Goals.Scored.Form = HData$Away.Goals.Scored.Form
HData$Team.Goals.Conceded.Form = HData$Home.Goals.Conceded.Form
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Team.Corners.Form = HData$Home.Corners.Form
HData$Team.Fouls.Form = HData$Home.Team.Fouls.Form
HData$Team.Yellow.Cards = HData$Home.Yellow.Cards
HData$Team.Red.Cards = HData$Home.Red.Cards
HData$Opposition.Corners.Form = HData$Away.Corners.Form
HData$Opposition.Fouls.Form = HData$Away.Team.Fouls.Form
HData$Opposition.Yellow.Cards = HData$Away.Yellow.Cards
HData$Opposition.Red.Cards = HData$Away.Red.Cards
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Relative.Goals.Form = ((HData$Team.Goals.Scored.Form - HData$Team.Goals.Conceded.Form) - (HData$Opposition.Goals.Scored.Form - HData$Opposition.Goals.Conceded.Form))
HData$Team.Odds = HData$B365H
HData$Draw.Odds = HData$B365D
HData$Opposition.Odds = HData$B365A
HData$Team.AH.Odds = HData$Ave.AH.Home.Odds
HData$Team.Handicap = HData$Asian.Handicap
HData$Opposition.AH.Odds = HData$Ave.AH.Away.Odds
HData$Team_Tier = HData$Home_Tier
HData$Opposition_Tier = HData$Away_Tier
HData$Team.Goal.Diff = HData$Full.Time.Home.Goals - HData$Full.Time.Away.Goals
HData$Home.Away = rep("Home",nrow(HData))

#-now create the data for the away matches
AData <- DATA[DATA$AwayTeam == Teams[i,1],]
#-Create the variables that we need - first for all the Away matches
AData$Team.Favourite = AData$Away.Favourite
AData$Opposition = AData$HomeTeam
AData$Team.Form = AData$Away.form
AData$Opposition.Form = AData$Home.Form
AData$Team.Shots.on.Target.Form = AData$Away.Shots.on.Target.Form
AData$Opposition.Shots.on.Target.Form = AData$Home.Shots.on.Target.Form
AData$Team.Shots.Conceded.Form = AData$Away.Shots.Conceded.Form
AData$Opposition.Shots.Conceded.Form = AData$Home.Shots.Conceded.Form
AData$Team.Goals.Scored.Form = AData$Away.Goals.Scored.Form
AData$Opposition.Goals.Scored.Form = AData$Home.Goals.Scored.Form
AData$Team.Goals.Conceded.Form = AData$Away.Goals.Conceded.Form
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Team.Corners.Form = AData$Away.Corners.Form
AData$Team.Fouls.Form = AData$Away.Team.Fouls.Form
AData$Team.Yellow.Cards = AData$Away.Yellow.Cards
AData$Team.Red.Cards = AData$Away.Red.Cards
AData$Opposition.Corners.Form = AData$Home.Corners.Form
AData$Opposition.Fouls.Form = AData$Home.Team.Fouls.Form
AData$Opposition.Yellow.Cards = AData$Home.Yellow.Cards
AData$Opposition.Red.Cards = AData$Home.Red.Cards
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Relative.Goals.Form = ((AData$Team.Goals.Scored.Form - AData$Team.Goals.Conceded.Form) - (AData$Opposition.Goals.Scored.Form - AData$Opposition.Goals.Conceded.Form))
AData$Team.Odds = AData$B365A
AData$Draw.Odds = AData$B365D
AData$Opposition.Odds = AData$B365H
AData$Team.AH.Odds = AData$Ave.AH.Away.Odds
AData$Team.Handicap = AData$Asian.Handicap*-1
AData$Opposition.AH.Odds = AData$Ave.AH.Home.Odds
AData$Team_Tier = AData$Away_Tier
AData$Opposition_Tier = AData$Home_Tier
AData$Team.Goal.Diff = AData$Full.Time.Away.Goals - AData$Full.Time.Home.Goals
AData$Home.Away = rep("Away",nrow(AData))

#-brill now we're cooking
#-so now that we have those two datasets let's append them and add them to our major dataframe
ComboData <- HData
ComboData <- rbind(ComboData,AData)
Team <- rep(Teams[i,1],nrow(ComboData))
ComboData <- cbind(ComboData,Team)

if(i == 1){
TeamData <- ComboData
}else{
TeamData <- rbind(TeamData,ComboData)
}

TeamData <- as.data.frame(TeamData)
}

#- ok so we have a tasty little treat here where we have to order the teamdata dataframe by teams seasons and gameweeks and then
#- we have to create a lookup table which calculates our basic strength, join that to our original
#- then create a win streak count and subsequent probability variable
#- also for shits and giggles as this is just the data manipulation stage I'm going to play with data.table
PreppedData <- data.table(TeamData)
PreppedData <- PreppedData[order(Team,Season,Game.Week.Index)]
#-create the points the team scored
PreppedData$Team.Points <- ifelse(PreppedData$Team.Goal.Diff > 0, 3, ifelse(PreppedData$Team.Goal.Diff < 0, 0, 1))
#-creating the lookup table from the average over the first 6 games
AveStr <- PreppedData[Game.Week.Index <= 6,.(Initial.Strength = ave(Team.Points)), by =.(Season, Team)]
OppStr <- PreppedData[Game.Week.Index <= 6,.(Opp.Initial.Strength = ave(Team.Points)), by =.(Season, Opposition)]
#- thar be duplicates ahead, remove 'em
#-remove the key
setkey(AveStr,NULL)
AveStr <- unique(AveStr)
setkey(OppStr,NULL)
OppStr <- unique(OppStr)

PreppedData <- join(PreppedData, AveStr, by = c("Season", "Team"))
PreppedData <- join(PreppedData, OppStr, by = c("Season", "Opposition"))
#-kl
#- nope it'll never catch on I mean "cool"
#-ok now I want to add a winning streak variable
#-for this one I'm going to have to cheat just a little bit as we need the average streak per season what we can then do is
#- look at the average ratio of first 6 games average to season and see if there is any consistency there if so we can use the first 6
#- as a proxy for the season that we are going to be predicting
#-so for this bit of code we set up an empty data table and then set it to the filtered PreppedData for each team season combo
#- Then we add another column which will count a 1 or 0 if the team has not lost or has lost
#- subsequently we create a second column which adds up these 1's and 0's into a streak

WinStrC <- data.table()
WinStreakComplete <- data.table()
for (e in 1:nrow(AveStr)){
WinStrC <- PreppedData[Team == AveStr[[e,2]] & Season == AveStr[[e,1]],]
		for (r in 1:nrow(WinStrC)){
		WinStrC$Win.ID <- ifelse(WinStrC$Team.Goal.Diff >= 0, 1, 0)
		if(r == 1){
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, 1,0)
			}else{
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, WinStrC$Win.Count[[r-1]]+1,0)
			}
		}
		if(r == 1){
		WinStreakComplete <- WinStrC
		}else{
			WinStreakComplete <- rbind(WinStreakComplete,WinStrC)
			}

}
ncol(WinStreakComplete)
PreppedData <- WinStreakComplete
rm(WinStreakComplete)
#-now figure out what the average streak is for each team across the model time period
AveStreak <- PreppedData[,.(Ave.Streak = ave(Win.Count)), by =.(Team)]
AveStreak <- unique(AveStreak)
PreppedData <- join(PreppedData, AveStreak, by = c("Team"))

#-finally ready to add on what the chances are of increasing the non lose streak
for(u in 1:nrow(PreppedData)){
	if(u == 1){
	PreppedData$Streak.Probability[[1]] <- dpois(PreppedData$Win.Count[[1]],PreppedData$Ave.Streak[[1]])
	}else{
	PreppedData$Streak.Probability[[u]] <- dpois(PreppedData$Win.Count[[u-1]]+1,PreppedData$Ave.Streak[[u]])
	}
}

#- save it as a csv so we can have a quick look in excel if we need to
#- we shouldn't need to
write.csv(PreppedData, "Europe Data For Modelling 2015 2016.csv")


#----------------------- Data Prep Over let's play with Models ---------------------------#
#-- You wouldn't go shopping without your wallet and bags, don't forget your packages
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


#- where we at
setwd ("C:/Users/coloughlin/Documents/Temp/Update/Football Predictions/Europe")
#setwd ("C:/Users/ciana/Documents/Temp/Update/Football Predictions/Europe")
 #-load in the data

 DATA <- read.csv("Europe Agg Data Input.csv", header = TRUE)

 #- so what we need to do is create a unique list of the teams involved in the latest season
 Teams <- DATA[DATA$Season == "2015 2016",5]
 Teams <- unique(Teams)
 Teams <- as.data.frame(Teams)
 TeamData <- data.frame


train <- read.csv ("Europe Data For Modelling 2015 2016.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)

#- Define two data frames (maybe they should be tables) which will store our results within loops and at the end of our loops
PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38 #- 38 games in a season son

#- some of the teams in the season that we are modelling won't have played in the division previously and as such will confuse our model, when
#- they are called, noting for it but to call on the spirit of Stalin and have them scrubbed from history
Teams <- Teams[Teams[,1] != "Angers",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Ajaccio GFCO",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Cardiff",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Crystal Palace",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Guingamp",]
Teams <- as.data.frame(Teams)

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
			ModTrain$Team_Tier <- ifelse(ModTrain$Team_Tier == 1, "Tier 1", ifelse(ModTrain$Team_Tier == 2, "Tier 2",ifelse(ModTrain$Team_Tier == 3, "Tier 3", "Tier 4" ) ))
			ModTrain$Opposition_Tier <- ifelse(ModTrain$Opposition_Tier == 1, "Tier 1", ifelse(ModTrain$Opposition_Tier == 2, "Tier 2",ifelse(ModTrain$Opposition_Tier == 3, "Tier 3", "Tier 4" ) ))
			ModTrain$Team_Description <- paste(ModTrain$Team_Tier,ModTrain$Opposition_Tier,ModTrain$Home.Away)

			for (q in 1 : nrow(ModTrain)){
			ModTrain$Expected.Goal.Difference[q] <- ((ave(rpois(50,ModTrain$Expected.Team.Goals[[q]]))[1] + ModTrain$Team.Handicap[[q]]) - (ave(rpois(50,ModTrain$Expected.Opposition.Goals[[q]]))[1]))
			}
			#- here we play a little with the dependent, kind of depends on the model we're running,in this case we want a trinomial classification
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff > 0, 2, ifelse(ModTrain$Team.Goal.Diff < 0, 0, 1))
			ModTrain$Team.Goal.Diff <- as.factor(ModTrain$Team.Goal.Diff)


			#- we have to feed it the input dataset of all the variables used, so define the formula below
			variables <- c("Season","Calendar_Season","Team_Description","Div","Team.Favourite","Expected.Goal.Difference","Team.Form","Opposition.Form","Team.Shots.on.Target.Form",
			"Opposition.Shots.Conceded.Form","Opposition.Shots.on.Target.Form","Team.Shots.Conceded.Form","Relative.Goals.Form",
			"Team.Odds","Draw.Odds","Streak.Probability","Asian.Handicap","Opposition.Odds","Relative.Odds","Expected.Shots","Relative.Form", "Team", "Game.Week.Index")
			TDat1 <- ModTrain[,variables]
			# take out the team and game week, we actually just need them later for the pred data
			TDat1 <- subset(TDat1, select=-c(Team, Game.Week.Index))
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
            trainTask <- makeClassifTask(data = TrainDat,target = "ModTrain.Team.Goal.Diff")

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
			makeNumericParam("colsample_bytree", lower = 0.2, upper = 0.8),
			makeDiscreteParam(id = "objective", values = c("multi:softprob"), tunable = F)
		  )
		  ctrl = makeTuneControlMBO()
		  inner = makeResampleDesc("Subsample", iters = 2)
		  lrn = makeTuneWrapper("classif.xgboost", resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

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
			  Score <- a$y.mmce.test.mean
			  eta <- a$x.eta
			  subsample <- a$x.subsample
			  min_child_weight <- a$x.min_child_weight
			  colsample_bytree <- a$x.colsample_bytree
			  objective <- "multi:softprob"
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
		  Model_Structure <- Agg_Results[which.max(Agg_Results$Score),]
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

		  TrainDat <- as.matrix(TrainDat)
		  Tester <- as.matrix(Tester)
		  dTrain <- xgb.DMatrix(TrainDat, label = Tester)
		  param <- list(max_depth = PM_max_depth, lambda = PM_lambda, eta = PM_eta, subsample = PM_subsample, min_child_weight = PM_min_child_weight,
						  colsample_bytree = PM_colsample_bytree)
		  Pmod <- xgb.train(param, dTrain, , nrounds = PM_nrounds, objective = PM_objective, num_class = 3)



		  #PredData <- train[train$Team == Teams[j,1] & train$Game.Week.Index == i & train$Season == "2015 2016",]
		  PredData <- train[train$Game.Week.Index == i & train$Season == "2015 2016",]
		  #- If any of the extra variables we created in Modtrain are used in the model we need to create them in PredData as well,so that happens here
		  PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
		  PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
		  PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
		  PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
		  PredData$Relative.Form <- PredData$Team.Form - PredData$Opposition.Form
		  PredData$Expected.Shots <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form) - (PredData$Team.Shots.Conceded.Form + PredData$Opposition.Shots.on.Target.Form)
		  PredData$Relative.Odds <- PredData$Opposition.Odds - PredData$Team.Odds
		  PredData$Team_Tier <- ifelse(PredData$Team_Tier == 1, "Tier 1", ifelse(PredData$Team_Tier == 2, "Tier 2",ifelse(PredData$Team_Tier == 3, "Tier 3", "Tier 4" ) ))
		  PredData$Opposition_Tier <- ifelse(PredData$Opposition_Tier == 1, "Tier 1", ifelse(PredData$Opposition_Tier == 2, "Tier 2",ifelse(PredData$Opposition_Tier == 3, "Tier 3", "Tier 4" ) ))
		  PredData$Team_Description <- paste(PredData$Team_Tier,PredData$Opposition_Tier,PredData$Home.Away)


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
		  # filter the dataset down a bit, first relevant team data
		  Team_ID <- paste0("Team",Teams[j,1])
		  PredDat <- PredDat[PredDat[,match(Team_ID, names(PredDat))] == 1, ]
		  PredDat <- as.matrix(PredDat[,-c(64:ncol(PredDat))])
			  PredDat <- xgb.DMatrix(PredDat)
			  PredData <- train[train$Team == Teams[j,1] & train$Game.Week.Index == i & train$Season == "2015 2016",]


#- Fit is a matrix of predicted values for each principle component
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


p1 <- PredData$Team
p1 <- as.data.frame(p1)
p2 <- "2015 2016"
p3 <- PredData$Opposition
p3 <- as.data.frame(p3)
p4 <- PredData$Game.Week.Index
p4 <- as.data.frame(p4)

			  #-now we are back to stitching our prediction table together
			  AggP <- cbind(p1,p2,p3,p4,P_Draw,P_Opposition,P_Team) #- stitched together
			  colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "P-Draw","P-Opposition","P-Team")

			  #- save the results
			  PredResults <- rbindlist(list(PredResults,AggP))

			  }
			  }

}
}



write.csv(PredResults, "Lily's Prediction 2015 2016 France.csv")
