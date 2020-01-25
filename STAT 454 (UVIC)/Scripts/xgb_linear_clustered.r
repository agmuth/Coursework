


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

for(i in 1:ncol(dist_mat)){
  
  t_start <- proc.time()
  
  #STAGE 1--------------------
  print(paste0("Fitting xgboost model to cluster ", i))
  
  
  wts <- 1 / (1 + dist_mat[,i]) #avoid distance of zero & weight obvs closer to cluster heavier
  
  #define task and learner
  task = makeRegrTask(data = train, target = "Response", weights=wts)
  learner = makeLearner("regr.xgboost")
  
  #set static parameter values for first stage
  learner$par.vals <- list(
    nrounds=30, 
    verbose=0,
    silent=0, #print out training log set to 0 for silent
    objective="reg:linear" #multi:softprob to get probabilities
  )
  
  ## Create Evaluation Function
  SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {
    preds = pred$data$response
    true = pred$data$truth 
    cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
    preds = as.numeric(Hmisc::cut2(preds, cuts))
    err = Metrics::ScoreQuadraticWeightedKappa(preds, true, 1, 8)
    return(-err)
  }
  
  SQWK = makeMeasure(id = "SQWK", minimize = FALSE, properties = c("regr"), best = 1, worst = 0,
                     fun = function(task, model, pred, feats, extra.args) {
                       return(-SQWKfun(x = seq(1.5, 7.5, by = 1), pred))
                     })
  
  ps = makeParamSet(
    makeNumericParam("eta", lower = 0.3, upper = 0.6),
    makeNumericParam("colsample_bytree", lower = 1, upper = 2, trafo = function(x) x/2),
    makeNumericParam("subsample", lower = 1, upper = 2, trafo = function(x) x/2),
    makeIntegerParam("max_depth", lower = 1, upper = 12) 
  )
  
  rdesc <- makeResampleDesc("CV", iters = 3L)
  ctrl <-  makeTuneControlGrid(resolution=4L)
  
  t1 <- proc.time()
  tune_res <- tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = SQWK)
  t2 <- proc.time()
  print(paste0("Grid search took ", round((t2-t1)[3] / 60, 1), " minutes"))
  
  saveRDS(tune_res, paste0("Tunes/tune_cluster", i, ".rds"))
  
  #STAGE 2--------------------
  
  
  learner_best <- makeLearner("regr.xgboost")
  
  #change number of boosting rounds - must be done before setting hyperparams
  learner_best$par.vals <- list(
    nrounds=100,
    silent=1, 
    objective="reg:linear"
  )
  
  #set optimal hyperparameters
  learner_best <- setHyperPars(learner_best, par.vals=tune_res$x)
  
  
  cv <- crossval(learner_best, task, iter = 3, measures = SQWK, show.info = TRUE)
  
  ## now try to find the optimal cutpoints that maximises the SQWK measure based on the Cross-Validated predictions
  optCuts <- optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = cv$pred)
  
  ## now train the model on all training data
  learner_best <- mlr::train(learner_best, task)
  
  #save learner 
  saveRDS(learner_best, paste0("Models/xgboost_cluster", i, ".rds"))
  saveRDS(optCuts, paste0("Models/cuts_cluster", i, ".rds"))
 
  t_stop <- proc.time()
  
  print(paste("Total time: ", round((t_stop-t_start)[3] / 60, 1), " minutes"))

}


