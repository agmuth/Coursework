library(caret)
library(DMwR)
library(data.table)
library(dplyr)

setwd("~/Undergraduate Work/STAT 454/Project")
source("codeBank.r")

train <- read.csv("train.csv") %>% clean(.)
test <- read.csv("test.csv") %>% clean(.)

set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")
split1 <- createDataPartition(train$Response, times=1, p=5/6, list=FALSE)

set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")
split2 <- createDataPartition(train$Response[split1], times=1, p=5/6, list=FALSE)

train_trees <- train[split1,]
test_trees <- train[-split1,]

train_lda <- train_trees[split2,]
test_lda <- train_trees[-split2,]

fwrite(train, "Cleaned Data/train.csv")
fwrite(test, "Cleaned Data/test.csv")

fwrite(train_trees, "Cleaned Data/train_trees.csv")
fwrite(test_trees, "Cleaned Data/test_trees.csv")

fwrite(train_lda, "Cleaned Data/train_lda.csv")
fwrite(test_lda, "Cleaned Data/test_lda.csv")

