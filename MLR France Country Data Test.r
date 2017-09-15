# this file tests a fully automated season prediction for one team in France

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



#-which project folder we want to work in
setwd ("C:/Users/coloughlin/Documents/Temp/Update/Football Predictions/France/Model Data")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

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
write.csv(PreppedData, "France Data For Modelling 2015 2016.csv")


#----------------------- Data Prep Over let's play with Models ---------------------------#

#- where we at
setwd ("C:/Users/coloughlin/Documents/Temp/Update/Football Predictions/France/Model Data")
 #-load in the data
train <- read.csv ("France Data For Modelling 2015 2016.csv", header=TRUE)
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

		for (j in 8:GWRange){ #- and for every gameweek
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Game.Week.Index > 6 & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Game.Week.Index > 6 & train$Game.Week.Index < 10 & train$Season == "2015 2016",]
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

			for (q in 1 : nrow(ModTrain)){
			ModTrain$Expected.Goal.Difference[q] <- ((ave(rpois(50,ModTrain$Expected.Team.Goals[[q]]))[1] + ModTrain$Team.Handicap[[q]]) - (ave(rpois(50,ModTrain$Expected.Opposition.Goals[[q]]))[1]))
			}
			#- here we play a little with the dependent, kind of depends on the model we're running,in this case we want a trinomial classification
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff > 0, 1, ifelse(ModTrain$Team.Goal.Diff <0, -1, 0))
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff == 1, "Team", ifelse(ModTrain$Team.Goal.Diff == 0, "Draw", "Opposition"))
			ModTrain$Team.Goal.Diff <- as.factor(ModTrain$Team.Goal.Diff)


			#- for RSNNS we have to feed it the input dataset of all the variables used, so define the formula below
			variables <- c("Season","Month","Team.Favourite","Expected.Goal.Difference","Team.Form","Opposition.Form","Team.Shots.on.Target.Form",
			"Opposition.Shots.Conceded.Form","Opposition.Shots.on.Target.Form","Team.Shots.Conceded.Form","Relative.Goals.Form","Home.Away","Opposition","Team",
			"Team.Odds","Draw.Odds","Streak.Probability","Asian.Handicap","Opposition.Odds","Relative.Odds","Expected.Shots","Relative.Form")
			TDat1 <- ModTrain[,variables]
			#- but on top of that you can't have categorical variables within the X matrix so to speak
			#- so we use a function to turn our categorical stuff into numeric data
			TDat2 <- dummyVars("~.",data=TDat1)
			TrainDat <- data.frame(predict(TDat2, newdata = TDat1))
			#- now we need to normalize the training data
			TrainDatnames <- colnames(TrainDat)
			TrainDat <- normalizeData(TrainDat, type="0_1")
			colnames(TrainDat) <- TrainDatnames

			#- same for Test data
			Tester <- data.frame(ModTrain$Team.Goal.Diff)
			TDat2 <- dummyVars("~.",data=Tester)
			TestDat <- data.frame(predict(TDat2, newdata = Tester))
			#- now we need to normalize the training data
			TestDatnames <- colnames(TestDat)
			TestDat <- normalizeData(TestDat, type="0_1")
			colnames(TestDat) <- TestDatnames



            # for this package it wants a single column with the the three groups to classify
            # so join this to TrainDat and use instead of TestDat
            TrainDat <- cbind(TrainDat, Tester)

            # ok so to start modelling we have to declare a so called 'task', with the
            # dependent and independent variables called out
            trainTask <- makeClassifTask(data = TrainDat,target = "ModTrain.Team.Goal.Diff")

            Agg_Results <- data.frame()

            #------------------------------ Std_Backpropagation, BackpropBatch and Time Delay Back Propagation ------------------------------# 6 minutes
            ## Tuning in inner resampling loop

            parallelStartSocket(3)

            ptm <- proc.time()
            ps = makeParamSet(
              makeIntegerVectorParam("size", len = 3, lower = 1, upper = 30),
              makeIntegerParam("maxit", lower = 500, upper = 500, tunable=F),
              makeDiscreteParam("initFunc", values = c("Randomize_Weights"), tunable = F),
              makeNumericVectorParam("learnFuncParams", len = 2, lower = c(0.1,0), upper = c(2,0.2), default = c(1.1,0.2)),
              makeDiscreteParam(id = "hiddenActFunc", values = c("Act_Logistic", "Act_TanH", "Act_Signum", "Act_Exponential", "Act_Euclid", "Act_Perceptron", "Act_Softmax", "Act_Identity", "Act_StepFunc",
                                        "Logistic_notInhibit", "Act_IdentityPlusBias", "Act_RM", "Act_Elliott", "Act_HystStep", "Act_LogisticTbl"), default = "Act_Softmax"),
              makeDiscreteParam(id = "updateFunc", values = c("Topological_Order", "Serial_Order", "Synchronous_Order", "Random_Order", "TimeDelay_Order")),
              makeDiscreteParam(id = "learnFunc", values = c("Std_Backpropagation", "TimeDelayBackprop", "BackpropBatch"), default = "Std_Backpropagation")
            )
            ctrl = makeTuneControlMBO()
            inner = makeResampleDesc("Subsample", iters = 2)
            lrn = makeTuneWrapper("classif.mlp", resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

            ## Outer resampling loop
            outer = makeResampleDesc("CV", iters = 3)
            r = resample(lrn, trainTask, resampling = outer, extract = getTuneResult, show.info = FALSE)
            parallelStop()
            proc.time()-ptm

            # Summarize fit and optimal parameters
            # this is a little messy but we summarize the optimal fits
            for (p in 1:length(r$extract)){
                a <- unlist(r$extract[[p]])
                size1 <- a$x.size1
                size2 <- a$x.size2
                size3 <- a$x.size3
                Score <- a$y.mmce.test.mean
                Activf <- a$x.hiddenActFunc
                updatef <- a$x.updateFunc
                Learnf <- a$x.learnFunc
                LearnP1 <- a$x.learnFuncParams1
                LearnP2 <- a$x.learnFuncParams2
                initF <- "Randomize_Weights"
            Classifier_Results <- as.data.frame(cbind(Learnf, size1, size2, size3, Score, updatef, Activf, LearnP1, LearnP2, initF))
            Agg_Results <- rbindlist(list(Agg_Results,Classifier_Results))
            }

            #------------------------------ BackPercolation ------------------------------# 6 mins
            ## Tuning in inner resampling loop

            parallelStartSocket(2)

            ptm <- proc.time()
            ps = makeParamSet(
              makeIntegerVectorParam("size", len = 3, lower = 1, upper = 30),
              makeDiscreteParam("initFunc", values = c("Randomize_Weights_Perc"), tunable = F),
              makeDiscreteParam("learnFunc", values = c("BackPercolation"), tunable = F),
              makeNumericVectorParam("learnFuncParams", len = 3, upper = c(10,0.2,0.2), lower = c(1,0.2,0.2), default = c(5,0.2,0.2)),
              makeDiscreteParam("hiddenActFunc", values = c("Act_TanH_Xdiv2"), tunable = F),
              makeDiscreteParam(id = "updateFunc", values = c("Topological_Order", "Serial_Order", "Synchronous_Order"))
            )
            ctrl = makeTuneControlMBO()
            inner = makeResampleDesc("Subsample", iters = 2)
            lrn = makeTuneWrapper("classif.mlp", resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

            ## Outer resampling loop
            outer = makeResampleDesc("CV", iters = 3)
            r = resample(lrn, trainTask, resampling = outer, extract = getTuneResult, show.info = FALSE)
            parallelStop()
            proc.time()-ptm

            # Summarize fit and optimal parameters
            # this is a little messy but we summarize the optimal fits
            for (p in 1:length(r$extract)){
                a <- unlist(r$extract[[p]])
                size1 <- a$x.size1
                size2 <- a$x.size2
                size3 <- a$x.size3
                Score <- a$y.mmce.test.mean
                Activf <- a$x.hiddenActFunc
                updatef <- a$x.updateFunc
                Learnf <- a$x.learnFunc
                LearnP1 <- a$x.learnFuncParams1
                LearnP2 <- a$x.learnFuncParams2
                LearnP3 <- a$x.learnFuncParams3
                initF <- "Randomize_Weights_Perc"
            Classifier_Results <- as.data.frame(cbind(Learnf, size1, size2, size3, Score, updatef, Activf, LearnP1, LearnP3, LearnP2, initF))
            Agg_Results <- rbindlist(list(Agg_Results,Classifier_Results), fill=T)
            }

            #------------------------------ BPTT and BBPTT ------------------------------# 6 mins
            ## Tuning in inner resampling loop

            parallelStartSocket(2)

            ptm <- proc.time()
            ps = makeParamSet(
              makeIntegerVectorParam("size", len = 3, lower = 1, upper = 30),
              makeNumericVectorParam("learnFuncParams", len = 3, lower = c(0.005,0,0), upper = c(0.1,1,5), default = c(0.005,0.5, 3)),
              makeDiscreteParam("initFunc", values = c("Randomize_Weights"), tunable = F),
              makeDiscreteParam(id = "hiddenActFunc", values = c("Act_Logistic", "Act_TanH", "Act_Signum", "Act_Exponential", "Act_Euclid", "Act_Perceptron", "Act_Softmax", "Act_Identity"), default = "Act_Softmax"),
              makeDiscreteParam(id = "updateFunc", values = c("Topological_Order", "Serial_Order", "Synchronous_Order")),
              makeDiscreteParam(id = "learnFunc", values = c("BPTT", "BBPTT"), default = "BPTT")
            )
            ctrl = makeTuneControlMBO()
            inner = makeResampleDesc("Subsample", iters = 2)
            lrn = makeTuneWrapper("classif.mlp", resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

            ## Outer resampling loop
            outer = makeResampleDesc("CV", iters = 3)
            r = resample(lrn, trainTask, resampling = outer, extract = getTuneResult, show.info = FALSE)
            parallelStop()
            proc.time()-ptm

            # Summarize fit and optimal parameters
            # this is a little messy but we summarize the optimal fits
            for (p in 1:length(r$extract)){
                a <- unlist(r$extract[[p]])
                size1 <- a$x.size1
                size2 <- a$x.size2
                size3 <- a$x.size3
                Score <- a$y.mmce.test.mean
                Activf <- a$x.hiddenActFunc
                updatef <- a$x.updateFunc
                Learnf <- a$x.learnFunc
                LearnP1 <- a$x.learnFuncParams1
                LearnP2 <- a$x.learnFuncParams2
                LearnP3 <- a$x.learnFuncParams3
                initF <- "Randomize Weights"
            Classifier_Results <- as.data.frame(cbind(Learnf, size1, size2, size3, Score, updatef, Activf, LearnP1, LearnP3, LearnP2, initF))
            Agg_Results <- rbindlist(list(Agg_Results,Classifier_Results), fill=T)
            }

            #---------------------------- check the winner and run a model on the full training set ----------------------------#
            # transform the Agg_Results so that the formatting is ok

            Agg_Results <- as.data.frame(Agg_Results)
            for(l in c(2,3,4,5,8,9,11)){
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
            Layers <- c(Model_Structure[1,2], Model_Structure[1,3], Model_Structure[1,4])
            PM_initFunc <- as.character(Model_Structure[1,10])
            PM_Learn <- as.character(Model_Structure[1,1])
            PM_Act <- as.character(Model_Structure[1,7])
            PM_Update <- as.character(Model_Structure[1,6])
            # Different algorithms need different parameters so we set up an if statement to choose the right structure
            if(Model_Structure[1,1] == "Std_Backpropagation" | Model_Structure[1,1] == "TimeDelayBackprop" | Model_Structure[1,1] == "BackpropBatch"){
                Learn_P <- c(Model_Structure[1,8],Model_Structure[1,9])
            }else{
                Learn_P <- c(Model_Structure[1,8],Model_Structure[1,9],Model_Structure[1,11])
            }

            Pmod <- mlp(TrainDat,TestDat,size=Layers, maxit=500, initFunc = PM_initFunc, learnFunc = PM_Learn , learnFuncParams = Learn_P, hiddenActFunc = PM_Act, updateFunc = PM_Update)




			PredData <- train[train$Game.Week.Index == 10 & train$Season == "2015 2016",]

			#- If any of the extra variables we created in Modtrain are used in the model we need to create them in PredData as well,so that happens here
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			PredData$Relative.Form <- PredData$Team.Form - PredData$Opposition.Form
			PredData$Expected.Shots <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form) - (PredData$Team.Shots.Conceded.Form + PredData$Opposition.Shots.on.Target.Form)
			PredData$Relative.Odds <- PredData$Opposition.Odds - PredData$Team.Odds

			#-sometimes a team won't be playing in the subsequent gameweek so this database will be empty,so move on folks nothing to see here
			if(nrow(PredData) > 0){
			PredData$Expected.Team.Goals <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form)
			PredData$Expected.Opposition.Goals <- (PredData$Opposition.Shots.on.Target.Form + PredData$Team.Shots.Conceded.Form)
			for (q in 1 : nrow(PredData)){
			PredData$Expected.Goal.Difference[q] <- (ave(rpois(50,PredData$Expected.Team.Goals[[q]]))[1]  + PredData$Team.Handicap[[q]]) - ave(rpois(50,PredData$Expected.Opposition.Goals[[q]]))[1]
			}

			PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff > 0, 1, ifelse(PredData$Team.Goal.Diff <0, -1, 0))
			PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff == 1, "Team", ifelse(PredData$Team.Goal.Diff == 0, "Draw", "Opposition"))
			PredData$Team.Goal.Diff <- as.factor(PredData$Team.Goal.Diff)


			if( PredData$Opposition %in% ModTrain$Opposition){
			#- we need to set up PredData to be in the same format as the Training and Test data
			PDat1 <- PredData[,variables]
			#- but on top of that you can't have categorical variables within the X matrix so to speak
			#- so we use a function to turn our categorical stuff into numeric data
			PDat2 <- dummyVars("~.",data=PDat1)
			PredDat <- data.frame(predict(PDat2, newdata = PDat1))
			#- now we need to normalize the training data
			PredDatnames <- colnames(PredDat)
			PredDat <- normalizeData(PredDat, type="0_1")
			colnames(PredDat) <- PredDatnames

				p1 <- PredData$Team
				p1 <- as.data.frame(p1)
				p2 <- "2015 2016"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)




#- Fit is a matrix of predicted values for each principle component
#-Act is the actual result in terms of goal difference
Fit <- predict(Pmod, PredDat)
Fit <- as.data.table(Fit)

				#-now we are back to stitching our prediction table together
				AggP <- cbind(p1,p2,p3,p4,Fit) #- stitched together
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "P-Draw","P-Opposition","P-Team")

				#- save the results
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}

				}
				}
		}



write.csv(PredResults, "Lily's Prediction 2015 2016 Single Round.csv")
