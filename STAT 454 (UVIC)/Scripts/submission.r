library(mlr)
library(xgboost)
library(dplyr)
library(data.table)
library(Hmisc)

setwd("~/Undergraduate Work/STAT 454/Project")
source("codebank.r")

test <- fread(file="Cleaned Data/test.csv")
test <- as.data.frame(test) #fread reads data in as a data.table must convert to data.frame for mlr
ID <- test$Id
test  <- dplyr::select(test, -Id)



dist_mat <- read.csv("test_kmeans_dist.csv")[,-c(1, 2)]
wts <- dist_mat / rowSums(dist_mat)

m <- list()
c <- list()
for(i in 1:10){
  m[[i]] <- readRDS(paste0("Final Model/xgboost_cluster", i, ".rds"))
  c[[i]] <- readRDS(paste0("Final Model/cuts_cluster", i, ".rds"))
}

preds_cluster <- data.frame(matrix(NA, nrow(test), 10))

for(i in 1:10){
  preds_cluster[,i] <- as.factor(as.numeric(Hmisc::cut2(predict(m[[i]], newdata=test)$data$response, c(-Inf, c[[i]]$par, Inf)))) 
}

preds <- as.factor(round(rowSums(as.numeric(as.matrix(preds_cluster)) * as.matrix(wts))))
write_submission(ID, preds, "submission.csv")