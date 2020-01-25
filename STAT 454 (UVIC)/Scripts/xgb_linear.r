# Created by Giuseppe Casalicchio
library(Metrics)
library(Hmisc)
library(xgboost)
library(checkmate)
library(mlr) 

setwd("~/Undergraduate Work/STAT 454/Project")
source("codebank.r")

#STAGE 1--------------------
t_start <- proc.time()

#load in and format data
train <- fread(file="Cleaned Data/train_trees.csv")
train <- as.data.frame(train) #fread reads data in as a data.table must convert to data.frame for mlr
train  <- dplyr::select(train, -Id)


## create mlr task and convert factors to dummy features
task = makeRegrTask(data = train, target = "Response")
learner = makeLearner("regr.xgboost")

learner$par.vals <- list(
  nrounds=30, 
  #max_depth=12, #use heuristic that depth should not exceed floor(sqrt(ncol(data)))
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
  #makeIntegerParam("min_child_weight", lower = 500, upper = 2000)
)

rdesc <- makeResampleDesc("CV", iters = 3L)
ctrl <-  makeTuneControlGrid(resolution=4L)


t1 <- proc.time()
tune_res <- tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = SQWK)
t2 <- proc.time()
print(paste0("Grid search took ", round((t2-t1)[3] / 60, 1), " minutes"))

saveRDS(tune_res, "Tunes/tune_linear.rds")

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
saveRDS(learner_best, "Models/xgboost_linear.rds")
saveRDS(optCuts, "Models/cuts_linear.rds")
t_stop <- proc.time()

print(paste("Total time: ", round((t_stop-t_start)[3] / 60, 1), " minutes"))


# 
# ## predict using the optimal cut-points 
# pred = predict(tr, testTask)
# preds = as.numeric(Hmisc::cut2(pred$data$response, c(-Inf, optCuts$par, Inf)))
# table(preds)
# 
# ## create submission file
# submission = data.frame(Id = testId)
# submission$Response = as.integer(preds)
# write.csv(submission, "mlr.xgboost.beatbench.csv", row.names = FALSE)