


library(Metrics)
library(Hmisc)
library(xgboost)
library(checkmate)
library(mlr) 
library(data.table)

setwd("~/Undergraduate Work/STAT 454/Project")
source("codebank.r")

#load in and format data
train <- fread(file="Cleaned Data/train_trees.csv")
train <- as.data.frame(train) #fread reads data in as a data.table must convert to data.frame for mlr


dist_mat <- fread(file="train_kmeans_dist.csv")
dist_mat <- as.data.frame(dist_mat)

#obvs are in same order - can check using commented line of code below
#sum(train$Id != dist_mat$Id)

train <- dplyr::select(train, -Id)
dist_mat <- dplyr::select(dist_mat, -Id)

for(i in 8:ncol(dist_mat)){
  
  t_start <- proc.time()
  
  #STAGE 1--------------------
  print(paste0("Fitting xgboost model to cluster ", i))
  
  
  wts <- 1 / (1 + dist_mat[,i]) #avoid distance of zero & weight obvs closer to cluster heavier
  
  
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
  
  saveRDS(tune_res, paste0("Tunes/tune_all_cluster", i, ".rds"))
  
  
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
  saveRDS(learner_best, paste0("Models/xgboost_all_cluster", i, ".rds"))
  t_stop <- proc.time()
  
  print(paste("Total time: ", round((t_stop-t_start)[3] / 60, 1), " minutes"))
  
}


