#USEFULL LINKS
#https://www.kaggle.com/casalicchio/use-the-mlr-package-scores-0-649
#https://mlr.mlr-org.com/

#'Script fits an xgboost classification model to train_trees data and then saves it. 
#'This is done in two stages. In the first stage 3-fold crossvalidation is 
#'used to evaluate combinations of hyper parameters. the number of boosting rounds for 
#'each xgboost model in this stage is 30. the numbers 3 and 30 were chosen to allow 
#'for a more time efficient search of the parameter space. In particular from playing
#'around with the parameter nrounds it was observed that improvement slows significantly
#'around the 30th boosting round. In the second stage the model is refit using the best 
#'hyperparameters and 100 boosting rounds. 

t_start <- proc.time()

library(mlr)
library(xgboost)
library(dplyr)
library(data.table)

setwd("~/Undergraduate Work/STAT 454/Project")
source("codebank.r")

#STAGE 1--------------------

#load in and format data
train <- fread(file="Cleaned Data/train_trees.csv")
train <- as.data.frame(train) #fread reads data in as a data.table must convert to data.frame for mlr
train$Response <- factor(train$Response) #response is read in as integer
train  <- dplyr::select(train, -Id)

wts <- get_weights(train$Response)


#define task and learner
task <- makeClassifTask(data=train, target="Response", weights=wts)
learner <- makeLearner("classif.xgboost", predict.type="prob")

#set static parameter values for first stage
learner$par.vals <- list(
  nrounds=30, 
  #max_depth=12, #use heuristic that depth should not exceed floor(sqrt(ncol(data)))
  verbose=0,
  silent=0, #print out training log set to 0 for silent
  eval_metric=SQWK_xgb,
  objective="multi:softprob" #multi:softprob to get probabilities
)

#define parameter space
ps = makeParamSet(
  makeNumericParam("eta", lower = 0.3, upper = 0.6),
  makeNumericParam("colsample_bytree", lower = 1, upper = 2, trafo = function(x) x/2),
  makeNumericParam("subsample", lower = 1, upper = 2, trafo = function(x) x/2),
  makeIntegerParam("max_depth", lower = 1, upper = 12) 
  #makeIntegerParam("min_child_weight", lower = 500, upper = 2000)
)

#define cv evaluation measure
SQWK_measure <- makeMeasure(id="SQWK_measure", minimize=FALSE, properties=c("classif", "classif.multi"), best=1, worst=0, 
                            fun=function(task, model, pred, feats, extra.args){return(SQWK_mlr(pred))})

#define cv protocol
rdesc <- makeResampleDesc("CV", iters = 3L)
ctrl <-  makeTuneControlGrid(resolution=4L)

#run and time cv
t1 <- proc.time()
tune_res <- tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = SQWK_measure)
t2 <- proc.time()
print(paste0("Grid search took ", round((t2-t1)[3] / 60, 1), " minutes"))

saveRDS(tune_res, "Tunes/tune_all_wts.rds")


#STAGE 2--------------------


learner_best <- makeLearner("classif.xgboost", predict.type="prob")

#change number of boosting rounds - must be done before setting hyperparams
learner_best$par.vals <- list(
  nrounds=100,
  silent=1, 
  eval_metric=SQWK_xgb,
  objective="multi:softprob"
)

#set optimal hyperparameters
learner_best <- setHyperPars(learner_best, par.vals=tune_res$x)

#retrain learner on whole dataset with optimal hyper parameters and nrounds=100 
learner_best <- mlr::train(learner_best, task)

#save learner 
saveRDS(learner_best, "Models/xgboost_all_wts.rds")
t_stop <- proc.time()

print(paste("Total time: ", round((t_stop-t_start)[3] / 60, 1), " minutes"))
