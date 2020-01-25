############## k means clustering

library(mlr)
library(MASS)
library(dplyr)
library(Metrics)
library(ggplot2)
library(caret)
library(class)

#setwd("~/Undergraduate Work/STAT 454/Project")
source("codeBank.R")
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")

# read in train_trees.csv and test_trees.csv
train <- fread(file="Cleaned Data/train_trees.csv")
train <- as.data.frame(train)

train_ID <- train$Id
train  <- dplyr::select(train, -Id)


test <- fread(file="Cleaned Data/test_trees.csv")
test <- as.data.frame(test)

test_ID <- test$Id
test <- dplyr::select(test, -Id)

########### perform pca and plot

train_pca <- prcomp(dplyr::select(train, -Response), center=TRUE, scale=TRUE)
pca_plot <- ggplot(data.frame(x=1:161, y=cumsum(train_pca$sdev^2 / sum(train_pca$sdev^2))), 
                   aes(x, y)) + geom_point(color='blue')
pca_plot + labs(x="Number of Principal Components", y="Cumulative Proportion of Variance Explained", title="Principal Component Analysis")
