
#-- 2015 2016 --#

{

require(pls)


setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data/")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2015 2016",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)


TeamData <- data.frame


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

#- save it as a csv so we can have a quick look in excel
write.csv(TeamData, "France Data For Modelling 2015 2016.csv")

#- in excel add the month column now
#-load the data
   
setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data")

 #-load in the data
train <- read.csv ("France Data For Modelling 2015 2016.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)


#-ok so when it comes to the main show we'll do a recursive loop or a split apply divide thing but for now let's just do a for loop


PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38


Teams <- Teams[Teams[,1] != "Angers",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Ajaccio GFCO",]
Teams <- as.data.frame(Teams)

for (i in 1:nrow(Teams))
{
		for (j in 8:GWRange){
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Game.Week.Index < j & train$Season == "2015 2016",]
			ModTrain <- rbind(ModTrain1,ModTrain2)
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- (ModTrain$Opposition.Shots.on.Target.Form + ModTrain$Team.Shots.Conceded.Form)
			ModTrain$Expected.Goal.Difference <- ave(rpois(50,ModTrain$Expected.Team.Goals))[1] - ave(rpois(50,ModTrain$Expected.Opposition.Goals))[1]
			
			
			
			#-then and no bells or whistles here, we run the linear regression
			Pmod <- mvr(Team.Goal.Diff ~
										Season +
										Month +
										Team.Favourite + 
										Home.Away +
										Team.Favourite*Home.Away +
										Expected.Goal.Difference +
										(Team.Form - Opposition.Form) +
										((Team.Shots.on.Target.Form + Opposition.Shots.Conceded.Form) - 
										(Opposition.Shots.on.Target.Form + Team.Shots.Conceded.Form))*Home.Away +
										Opposition +
										(Team.Odds - Opposition.Odds), data=ModTrain)
			
			PredData <- train[train$Team == Teams[i,1] & train$Game.Week.Index == j & train$Season == "2015 2016",]
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			
			
			if(nrow(PredData) > 0){	
			PredData$Expected.Team.Goals <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form)
			PredData$Expected.Opposition.Goals <- (PredData$Opposition.Shots.on.Target.Form + PredData$Team.Shots.Conceded.Form)
			PredData$Expected.Goal.Difference <- ave(rpois(50,PredData$Expected.Team.Goals))[1] - ave(rpois(50,PredData$Expected.Opposition.Goals))[1]
			
			
			if( PredData$Opposition %in% ModTrain$Opposition){	
				p1 <- Teams[i,1]
				p1 <- as.data.frame(p1)
				p2 <- "2015 2016"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)
				
				
								
				
#- Fit is a matrix of predicted values for each principle component
#-Act is the actual result in terms of goal difference				
Fit <- predict(Pmod, ModTrain)
Fit <- as.data.frame(Fit)
Act <- ModTrain$Team.Goal.Diff
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

#-first column of our new dataframe will be the actual result
ResP1 <- data.frame()
ResP1 <- rep(1,nrow(Resers)) #- just create this as a temporary measure so that things are the right size
ResP1 <- as.data.frame(ResP1)
ResP1$Act <- ifelse(Resers$Act >0,1,ifelse(Resers$Act < 0, -1, 0)) #- classify the actual goal difference into the results
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
					for (m in 2:ncol(TempSt)){
					TempStp1[,m-1] <- TempSt[1,m] - TempSt[1,m-1]
					}
					
					M2 <- match(max(TempStp1),TempStp1)+1 #- it's plus one because the first column is ignored because it can't be the elbow
					M3 <- match(M1, TempSt[2,]) #- what's the highest accuracy achieved by the principle components
					#- set up TempStp2 as a house for the summary information
					TempStp2 <- as.data.frame(1)
					TempStp2[,1] <- ModTrain$Team[1] #- the Team we're modelling
					TempStp2[,2] <- CValues[d,1] #- the positive C-Value
					TempStp2[,3] <- CValues[d,2] #- the negative C-Value
					TempStp2[,4] <- M1 #- the accuracy
					TempStp2[,5] <- M2 #- the Elbow P.C
					TempStp2[,6] <- M3 #- the maximum accuracy
					
					#-then if this is the first set of C-Values we're running through save ToSt as TempStp2 else tack it on at the end
					if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
					}


}
#-rename the matric that has the different results based on all the c-values
colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", "Max Accuracy", "Elbow P.C", "Max Acc P.C")

				PC1 <- match(max(ToSt[,4]),ToSt[,4]) #-this is the row where the maximum accuracy is across the c-values
ToStp <- ToSt[PC1,] #-ToStp is just going to be that row
#-this below bit is to make sure that if a team appears more than once in a gameweek we pick up all matches
ap <- nrow(PredData)
if(d > 1){
		for (g in 2:ap){
		ToStp[g,] <- ToStp[1,]
		}
}
				
				
				#-now we are back to stitching our prediction table together
				p5a <- predict(Pmod, PredData) #- this gives a prediction for each principle component
				p5a <- as.data.frame(p5a)
				p5b <- ncol(p5a)
				p5 <- p5a[,ToStp[1,5]] #- this select the elbow pc from the prediction matrix
				p5 <- as.data.frame(p5)
				AggP <- cbind(p1,p2,p3,p4,p5,ToStp[,2],ToStp[,3],ToStp[,4]) #- stitched together
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "Prediction", "Pos C-Value", "Neg C-Value", "Max Accuracy")
				
				#- save the results
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}
				
				}
				}
		}
}



write.csv(PredResults, "PC Reg Extended 2015 2016.csv")



}





#-- 2014 2015 --#



{

require(pls)


setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data/")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2014 2015",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)


TeamData <- data.frame


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

#- save it as a csv so we can have a quick look in excel
write.csv(TeamData, "France Data For Modelling 2014 2015.csv")

#- in excel add the month column now
#-load the data
   
setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data")

 #-load in the data
train <- read.csv ("France Data For Modelling 2014 2015.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)


#-ok so when it comes to the main show we'll do a recursive loop or a split apply divide thing but for now let's just do a for loop


PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38


#Teams <- Teams[Teams[,1] != "Angers",]
#Teams <- as.data.frame(Teams)
#Teams <- Teams[Teams[,1] != "Ajaccio GFCO",]
#Teams <- as.data.frame(Teams)

for (i in 1:nrow(Teams))
{
		for (j in 8:GWRange){
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Season != "2014 2015" & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Game.Week.Index < j & train$Season == "2014 2015",]
			ModTrain <- rbind(ModTrain1,ModTrain2)
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- exp(ModTrain$Team.Goals.Scored.Form - ModTrain$Opposition.Goals.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- exp(ModTrain$Opposition.Goals.Scored.Form - ModTrain$Team.Goals.Conceded.Form)
			ModTrain$Expected.Goal.Difference <- ave(rpois(150,ModTrain$Expected.Team.Goals))[1] - ave(rpois(150,ModTrain$Expected.Opposition.Goals))[1]
			
			
			
			#-then and no bells or whistles here, we run the linear regression
			Pmod <- mvr(Team.Goal.Diff/ave(Team.Goal.Diff) ~
										Season +
										Month +
										Team.Favourite + 
										Home.Away +
										Team.Favourite*Home.Away +
										(Team.Form - Opposition.Form)/ave(Team.Form - Opposition.Form) +
										Expected.Goal.Difference/ave(Expected.Goal.Difference) +
										Team.Goals.Conceded.Form/Opposition.Goals.Scored.Form/ave(Team.Goals.Conceded.Form/Opposition.Goals.Scored.Form) +
										Opposition.Goals.Conceded.Form/Team.Goals.Scored.Form/ave(Opposition.Goals.Conceded.Form/Team.Goals.Scored.Form) +
										Opposition +
										Team.Odds/ave(Team.Odds) +
										Draw.Odds/ave(Draw.Odds) +
										Opposition.Odds/ave(Opposition.Odds), data=ModTrain)
			
			PredData <- train[train$Team == Teams[i,1] & train$Game.Week.Index == j & train$Season == "2014 2015",]
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			
			
			if(nrow(PredData) > 0){	
			PredData$Expected.Team.Goals <- exp(PredData$Team.Goals.Scored.Form - PredData$Opposition.Goals.Conceded.Form)
			PredData$Expected.Opposition.Goals <- exp(PredData$Opposition.Goals.Scored.Form - PredData$Team.Goals.Conceded.Form)
			PredData$Expected.Goal.Difference <- ave(rpois(150,PredData$Expected.Team.Goals))[1] - ave(rpois(150,PredData$Expected.Opposition.Goals))[1]
			
			
			if( PredData$Opposition %in% ModTrain$Opposition){	
				p1 <- Teams[i,1]
				p1 <- as.data.frame(p1)
				p2 <- "2014 2015"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)
				
				
								
				
				
Fit <- predict(Pmod, ModTrain)
Fit <- as.data.frame(Fit)
Act <- ModTrain$Team.Goal.Diff

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

#-first column of our new dataframe will be the actual result
ResP1 <- data.frame()
ResP1 <- rep(1,nrow(Resers))
ResP1 <- as.data.frame(ResP1)
ResP1$Act <- ifelse(Resers$Act >0,1,ifelse(Resers$Act < 0, -1, 0))
ResP1 <- ResP1[,-1]
head(ResP1)
ResP1 <- as.data.frame(ResP1)



ToSt <- as.data.frame(1)
for (d in 1:nrow(CValues)){
		
		for(f in 2:ncol(Resers)){
		ResP1[,f] <- ifelse(Resers[,f] > CValues[d,1],1,ifelse(Resers[,f] < -1*CValues[d,2], -1, 0))
		}
			SuSt <- rep(1,a)
			SuSt <- as.data.frame(SuSt)
			for (k in 2:ncol(ResP1)){
			SuSt[,k-1] <- ifelse(ResP1[,k] == ResP1[,1],1,0)
			}
			TempSt <- as.data.frame(1)
				for (l in 1:ncol(SuSt)){
				TempSt[,l] <- sum(SuSt[,l])
				TempSt[2,l] <- TempSt[1,l] / nrow(SuSt)
				}
					M1 <- max(TempSt[2,])
					TempStp1 <- as.data.frame(1)
					for (m in 2:ncol(TempSt)){
					TempStp1[,m-1] <- TempSt[1,m] - TempSt[1,m-1]
					}
					M2 <- match(max(TempStp1),TempStp1)+1
					M3 <- match(M1, TempSt[2,])
					TempStp2 <- as.data.frame(1)
					TempStp2[,1] <- ModTrain$Team[1]
					TempStp2[,2] <- CValues[d,1]
					TempStp2[,3] <- CValues[d,2]
					TempStp2[,4] <- M1
					TempStp2[,5] <- M2
					TempStp2[,6] <- M3
					
					if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
					}

}
colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", "Max Accuracy", "Elbow P.C", "Max Acc P.C")
				PC1 <- match(max(ToSt[,4]),ToSt[,4])
ToStp <- ToSt[PC1,]
ap <- nrow(PredData)
if(d > 1){
		for (g in 2:ap){
		ToStp[g,] <- ToStp[1,]
		}
}
				
				
				
				p5a <- predict(Pmod, PredData)
				p5a <- as.data.frame(p5a)
				p5b <- ncol(p5a)
				p5 <- p5a[,ToStp[1,5]]
				p5 <- as.data.frame(p5)
				AggP <- cbind(p1,p2,p3,p4,p5,ToStp[,2],ToStp[,3],ToStp[,4])
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "Prediction", "Pos C-Value", "Neg C-Value", "Max Accuracy")
				
				
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}
				
				}
				}
		}
}



write.csv(PredResults, "PC Reg Extended 2014 2015.csv")



}



#-- 2013 2014 --#

{

require(pls)


setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data/")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2013 2014",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)


TeamData <- data.frame


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

#- save it as a csv so we can have a quick look in excel
write.csv(TeamData, "France Data For Modelling 2013 2014.csv")

#- in excel add the month column now
#-load the data
   
setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data")

 #-load in the data
train <- read.csv ("France Data For Modelling 2013 2014.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)


#-ok so when it comes to the main show we'll do a recursive loop or a split apply divide thing but for now let's just do a for loop


PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38


Teams <- Teams[Teams[,1] != "Guingamp",]
Teams <- as.data.frame(Teams)

for (i in 1:nrow(Teams))
{
		for (j in 8:GWRange){
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Season != "2013 2014" & train$Season != "2014 2015" & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Game.Week.Index < j & train$Season == "2013 2014",]
			ModTrain <- rbind(ModTrain1,ModTrain2)
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- exp(ModTrain$Team.Goals.Scored.Form - ModTrain$Opposition.Goals.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- exp(ModTrain$Opposition.Goals.Scored.Form - ModTrain$Team.Goals.Conceded.Form)
			ModTrain$Expected.Goal.Difference <- ave(rpois(150,ModTrain$Expected.Team.Goals))[1] - ave(rpois(150,ModTrain$Expected.Opposition.Goals))[1]
			
			
			
			#-then and no bells or whistles here, we run the linear regression
			Pmod <- mvr(Team.Goal.Diff ~
										Season +
										Month +
										Team.Favourite + 
										Home.Away +
										Team.Favourite*Home.Away +
										(Team.Form - Opposition.Form) +
										Expected.Goal.Difference +
										Team.Goals.Conceded.Form/Opposition.Goals.Scored.Form +
										Opposition.Goals.Conceded.Form/Team.Goals.Scored.Form +
										Opposition +
										Team.Odds +
										Draw.Odds +
										Opposition.Odds, data=ModTrain)
			
			PredData <- train[train$Team == Teams[i,1] & train$Game.Week.Index == j & train$Season == "2013 2014",]
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			
			
			if(nrow(PredData) > 0){	
			PredData$Expected.Team.Goals <- exp(PredData$Team.Goals.Scored.Form - PredData$Opposition.Goals.Conceded.Form)
			PredData$Expected.Opposition.Goals <- exp(PredData$Opposition.Goals.Scored.Form - PredData$Team.Goals.Conceded.Form)
			PredData$Expected.Goal.Difference <- ave(rpois(150,PredData$Expected.Team.Goals))[1] - ave(rpois(150,PredData$Expected.Opposition.Goals))[1]
			
			
			if( PredData$Opposition %in% ModTrain$Opposition){	
				p1 <- Teams[i,1]
				p1 <- as.data.frame(p1)
				p2 <- "2013 2014"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)
				
				
								
				
				
Fit <- predict(Pmod, ModTrain)
Fit <- as.data.frame(Fit)
Act <- ModTrain$Team.Goal.Diff

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

#-first column of our new dataframe will be the actual result
ResP1 <- data.frame()
ResP1 <- rep(1,nrow(Resers))
ResP1 <- as.data.frame(ResP1)
ResP1$Act <- ifelse(Resers$Act >0,1,ifelse(Resers$Act < 0, -1, 0))
ResP1 <- ResP1[,-1]
head(ResP1)
ResP1 <- as.data.frame(ResP1)



ToSt <- as.data.frame(1)
for (d in 1:nrow(CValues)){
		
		for(f in 2:ncol(Resers)){
		ResP1[,f] <- ifelse(Resers[,f] > CValues[d,1],1,ifelse(Resers[,f] < -1*CValues[d,2], -1, 0))
		}
			SuSt <- rep(1,a)
			SuSt <- as.data.frame(SuSt)
			for (k in 2:ncol(ResP1)){
			SuSt[,k-1] <- ifelse(ResP1[,k] == ResP1[,1],1,0)
			}
			TempSt <- as.data.frame(1)
				for (l in 1:ncol(SuSt)){
				TempSt[,l] <- sum(SuSt[,l])
				TempSt[2,l] <- TempSt[1,l] / nrow(SuSt)
				}
					M1 <- max(TempSt[2,])
					TempStp1 <- as.data.frame(1)
					for (m in 2:ncol(TempSt)){
					TempStp1[,m-1] <- TempSt[1,m] - TempSt[1,m-1]
					}
					M2 <- match(max(TempStp1),TempStp1)+1
					M3 <- match(M1, TempSt[2,])
					TempStp2 <- as.data.frame(1)
					TempStp2[,1] <- ModTrain$Team[1]
					TempStp2[,2] <- CValues[d,1]
					TempStp2[,3] <- CValues[d,2]
					TempStp2[,4] <- M1
					TempStp2[,5] <- M2
					TempStp2[,6] <- M3
					
					if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
					}

}
colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", "Max Accuracy", "Elbow P.C", "Max Acc P.C")
				PC1 <- match(max(ToSt[,4]),ToSt[,4])
ToStp <- ToSt[PC1,]
ap <- nrow(PredData)
if(d > 1){
		for (g in 2:ap){
		ToStp[g,] <- ToStp[1,]
		}
}
				
				
				
				p5a <- predict(Pmod, PredData)
				p5a <- as.data.frame(p5a)
				p5b <- ncol(p5a)
				p5 <- p5a[,ToStp[1,5]]
				p5 <- as.data.frame(p5)
				AggP <- cbind(p1,p2,p3,p4,p5,ToStp[,2],ToStp[,3],ToStp[,4])
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "Prediction", "Pos C-Value", "Neg C-Value", "Max Accuracy")
				
				
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}
				
				}
				}
		}
}



write.csv(PredResults, "PC Reg Extended 2013 2014.csv")


}