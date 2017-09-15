## MLR testing grounds
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

#- where we at
setwd ("C:/Users/coloughlin/Documents/Temp/Update/Football Predictions/France/Model Data")
 #-load in the data
train <- read.csv ("France Data For Modelling 2014 2015.csv", header=TRUE)
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

#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6
ModTrain1 <- train[train$Team == Teams[1,1] & train$Game.Week.Index > 6 & train$Season != "2014 2015" & train$Season != "2015 2016",]
ModTrain2 <- train[train$Team == Teams[1,1] & train$Game.Week.Index > 6 & train$Game.Week.Index < 10 & train$Season == "2014 2015",]
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
"Opposition.Shots.Conceded.Form","Opposition.Shots.on.Target.Form","Team.Shots.Conceded.Form","Relative.Goals.Form","Home.Away","Opposition",
"Team.Odds","Draw.Odds","Streak.Probability","Team.Handicap","Opposition.Odds","Relative.Odds","Expected.Shots","Relative.Form","Expected.Opposition.Goals",
"Expected.Team.Goals","High.Opposition.Form")
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

parallelStartSocket(2)

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

#------------------------------ Counterpropagation ------------------------------# 5 minutes
## Tuning in inner resampling loop
#parallelStartSocket(3)

#ptm <- proc.time()
#ps = makeParamSet(
#  makeIntegerVectorParam("size", len = 3, lower = 1, upper = 30),
#  makeNumericVectorParam("learnFuncParams", len=3, lower = c(0.1,0,0), upper = c(0.7,1,0), default=c(0.3,0.5,0)),
#  makeDiscreteParam(id = "initFunc", values = c("CPN_Weights_v3.3"), default = "CPN_Weights_v3.3"),
#  makeDiscreteParam("hiddenActFunc", values=c("Act_Signum"), default = "Act_Signum"),
#  makeDiscreteParam(id = "updateFunc", values = c("Topological_Order", "Serial_Order", "Synchronous_Order")),
#  makeDiscreteParam(id = "learnFunc", values = c("Counterpropagation"), default = "Counterpropagation")
#)
#ctrl = makeTuneControlMBO()
#inner = makeResampleDesc("Subsample", iters = 2)
#lrn = makeTuneWrapper("classif.mlp", resampling = inner, par.set = ps, control = ctrl, show.info = T)

## Outer resampling loop
#outer = makeResampleDesc("CV", iters = 3)
#r = resample(lrn, trainTask, resampling = outer, extract = getTuneResult, show.info = T)
#parallelStop()
#proc.time()-ptm

# Summarize fit and optimal parameters
# this is a little messy but we summarize the optimal fits
#for (p in 1:length(r$extract)){
#    a <- unlist(r$extract[[p]])
#    size1 <- a$x.size1
#    size2 <- a$x.size2
#    size3 <- a$x.size3
#    Score <- a$y.mmce.test.mean
#    Activf <- a$x.hiddenActFunc
#    updatef <- a$x.updateFunc
#    Learnf <- a$x.learnFunc
#    LearnP1 <- a$x.learnFuncParams1
#    LearnP2 <- a$x.learnFuncParams2
#    LearnP3 <- a$x.learnFuncParams3
#    initF <- a$x.initFunc
#Classifier_Results <- as.data.frame(cbind(Learnf, size1, size2, size3, Score, updatef, Activf, LearnP1, LearnP3, LearnP2, initF))
#Agg_Results <- rbindlist(list(Agg_Results,Classifier_Results), fill=T)
#}

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
TrainDat <- TrainDat[,-80]
# Define the ideal model structure
Layers <- c(Model_Structure[1,2], Model_Structure[1,3], Model_Structure[1,4])
PM_initFunc <- as.character(Model_Structure[1,10])
PM_Learn <- as.character(Model_Structure[1,1])
PM_Act <- as.character(Model_Structure[1,7])
PM_Update <- as.character(Model_Structure[1,6])
# Different algorithms need different parameters so we set up an if statement to choose the right structure
if(Model_Structure[1,1] == "Std_Backpropagation" | Model_Structure[1,1] == "TimeDelayBackprop" | Model_Structure[1,1] == "BackpropBatch" |){
    Learn_P <- c(Model_Structure[1,8],Model_Structure[1,9])
}else{
    Learn_P <- c(Model_Structure[1,8],Model_Structure[1,9],Model_Structure[1,11])
}

Pmod <- mlp(TrainDat,TestDat,size=Layers, maxit=500, initFunc = PM_initFunc, learnFunc = PM_Learn , learnFuncParams = Learn_P, hiddenActFunc = PM_Act, updateFunc = PM_Update)

# set up the algorithm on the full training data


#---------------------------- Bits that the above was plastered together with ----------------------------#


# ok so we can normalise the data if we need to but that's all ready been done
#trainTask <- normalizeFeatures(trainTask,method = "standardize")
# Can also drop variables here which is pretty handy and which I might take advantage of
#trainTask <- dropFeatures(task = trainTask,features = c("Loan_ID","Married.dummy"))

# there's also a yoke which calls out the feature importance - could be important
im_feat <- generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
plotFilterValues(im_feat,n.show = 20)
plotFilterValuesGGVIS(im_feat)
# you can also find out what function paramters are:
getParamSet("classif.randomForest")
getParamSet("classif.mlp")

# define a learner
rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))

# Set up 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

# Search for hyperparameters
rf_param <- makeParamSet(
    makeIntegerParam("ntree",lower = 50, upper = 500),
    makeIntegerParam("mtry", lower = 3, upper = 10),
    makeIntegerParam("nodesize", lower = 10, upper = 50)
)

# do a grid search
gscontrol <- makeTuneControlGrid()
# hypertuning
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)

## Seperate example from mlr website about nested resampling
## Tuning in inner resampling loop
ps = makeParamSet(
  makeDiscreteParam("C", values = 2^(-2:2)),
  makeDiscreteParam("sigma", values = 2^(-2:2))
)
ctrl = makeTuneControlGrid()
inner = makeResampleDesc("Subsample", iters = 2)
lrn = makeTuneWrapper("classif.ksvm", resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

## Outer resampling loop
outer = makeResampleDesc("CV", iters = 3)
r = resample(lrn, iris.task, resampling = outer, extract = getTuneResult, show.info = FALSE)

# You can obtain the error rates on the 3 outer test sets by:
r$measures.train
r$measures.test
r$extract

# With function getNestedTuneResultsOptPathDf you can extract the optimization paths for the 3 outer cross-validation iterations
opt.paths = getNestedTuneResultsOptPathDf(r)

# Another useful function is getNestedTuneResultsX, which extracts the best found hyperparameter settings for each outer resampling iteration
getNestedTuneResultsX(r)
res = benchmark(lrn, trainTask, outer, measures = list(acc, ber), show.info = FALSE)




#----------------- ok let's have a go with France data -----------------#

TrainDat <- cbind(TrainDat, Tester)

# ok so to start modelling we have to declare a so called 'task', with the
# dependent and independent variables called out
trainTask <- makeClassifTask(data = TrainDat,target = "ModTrain.Team.Goal.Diff")
# Define the learner
PMod <- makeLearner("classif.mlp", predict.type = "response", par.vals = list(size=c(10,5,5), maxit=600, initFunc = "Randomize Weights", learnFunc = "Std_Backpropagation", learnFuncParams = c(0.5,0.1), hiddenActFunc = "Act_TanH"))
Pmod <- mlp(TrainDat,TestDat,size=c(10), maxit=200, initFunc = "Randomize Weights", initFuncparams = c(0.5, -0.5), learnFunc = "Std_Backpropagation", learnFuncParams = c(0.5,0.1), hiddenActFunc = "Act_TanH")
## Tuning in inner resampling loop
require(parallelMap)
parallelStartSocket(3)

ptm <- proc.time()
ps = makeParamSet(
  makeIntegerVectorParam("size", len = 2, lower = 8, upper = 12),
  makeUntypedParam("hiddenActFunc", c("Act_TanH","Act_Softmax","Act_Elliott"))
)
ctrl = makeTuneControlGrid()
inner = makeResampleDesc("Subsample", iters = 2)
lrn = makeTuneWrapper("classif.mlp", resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

## Outer resampling loop
outer = makeResampleDesc("CV", iters = 3)
r = resample(lrn, trainTask, resampling = outer, extract = getTuneResult, show.info = FALSE)
parallelStop()
proc.time()-ptm
