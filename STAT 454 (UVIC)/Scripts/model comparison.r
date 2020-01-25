library(mlr)
library(xgboost)
library(dplyr)
library(data.table)
library(Hmisc)

setwd("~/Undergraduate Work/STAT 454/Project")
source("codebank.r")

#load in and format data
test <- fread(file="Cleaned Data/test_trees.csv")
test <- as.data.frame(test) #fread reads data in as a data.table must convert to data.frame for mlr
test$Response <- factor(test$Response) #response is read in as integer
test  <- dplyr::select(test, -Id)

preds <- as.data.frame(matrix(NA, nrow(test), 8))
colnames(preds) <- c("multi", "multi wieghted", "1 vs all", "1 vs all weighted", "linear", "cluster_linear", "cluster", "cluster_weighted")


m <- readRDS("Models/xgboost_all.rds")
preds[,1] <- predict(m, newdata=test)$data$response


m <- readRDS("Models/xgboost_all_wts.rds")
preds[,2] <- predict(m, newdata=test)$data$response


m <- list()
for(i in 1:8){m[[i]] <- readRDS(paste0("Models/xgboost_class", i, ".rds"))}
preds[,3] <- custom_predict(m, test)


m <- list()
for(i in 1:8){m[[i]] <- readRDS(paste0("Models/xgboost_class", i, "_wt.rds"))}
preds[,4] <- custom_predict(m, test)


m <- readRDS("Models/xgboost_linear.rds")
optCuts <- readRDS("Models/cuts_linear.rds")
preds[,5] <- as.factor(as.numeric(Hmisc::cut2(predict(m, newdata=test)$data$response, c(-Inf, optCuts$par, Inf))))


#ADDED DECEMBER 9TH------------------------

dist_mat <- read.csv("test_trees_kmeans_dist.csv")[,-c(1, 2)]
wts <- dist_mat / rowSums(dist_mat)
ind <- apply(dist_mat, 1, which.min)

m <- list()
cuts <- list()
for(i in 1:10){
  m[[i]] <- readRDS(paste0("Models/xgboost_cluster", i, ".rds"))
  cuts[[i]] <- readRDS(paste0("Models/cuts_cluster", i, ".rds"))
}

preds_cluster <- data.frame(matrix(NA, nrow(test), 10))

for(i in 1:10){
  preds_cluster[,i] <- as.factor(as.numeric(Hmisc::cut2(predict(m[[i]], newdata=test)$data$response, c(-Inf, cuts[[i]]$par, Inf)))) 
}

preds[,6] <- as.factor(round(rowSums(as.numeric(as.matrix(preds_cluster)) * as.matrix(wts))))


#cluster_classification

m <- list()
for(i in 1:10){
  m[[i]] <- readRDS(paste0("Models/xgboost_all_cluster", i, ".rds"))
}

preds_cluster <- data.frame(matrix(NA, nrow(test), 10))

for(i in 1:10){
  preds_cluster[,i] <- as.factor(as.numeric(predict(m[[i]], newdata=test)$data$response)) 
}

preds[,7] <- as.factor(round(rowSums(as.numeric(as.matrix(preds_cluster)) * as.matrix(wts))))


#cluster_classification wts

m <- list()
for(i in 1:10){
  m[[i]] <- readRDS(paste0("Models/xgboost_all_wts_cluster", i, ".rds"))
}

preds_cluster <- data.frame(matrix(NA, nrow(test), 10))

for(i in 1:10){
  preds_cluster[,i] <- as.factor(as.numeric(predict(m[[i]], newdata=test)$data$response)) 
}

preds[,8] <- as.factor(round(rowSums(as.numeric(as.matrix(preds_cluster)) * as.matrix(wts))))





#-----------

ScoreQuadraticWeightedKappa(test$Response, preds[,1])
ScoreQuadraticWeightedKappa(test$Response, preds[,2])
ScoreQuadraticWeightedKappa(test$Response, preds[,3])
ScoreQuadraticWeightedKappa(test$Response, preds[,4])
ScoreQuadraticWeightedKappa(test$Response, preds[,5])
ScoreQuadraticWeightedKappa(test$Response, preds[,6])
ScoreQuadraticWeightedKappa(test$Response, preds[,7])
ScoreQuadraticWeightedKappa(test$Response, preds[,8])


