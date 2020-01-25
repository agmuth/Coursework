library(mlr)
library(MASS)
library(dplyr)
library(Metrics)
library(ggplot2)
library(caret)

# setwd("~/Undergraduate Work/STAT 454/Project")
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")

train <- fread(file="Cleaned Data/train_lda.csv")
train <- as.data.frame(train)

train_ID <- train$Id
train  <- dplyr::select(train, -Id)

test <- fread(file="Cleaned Data/test_lda.csv")
test <- as.data.frame(test)

test_ID <- test$Id
test <- dplyr::select(test, -Id)

col_remove <- removeCorrelatedFeatures(train)
train <- dplyr::select(train, -col_remove)
test <- dplyr::select(test, -col_remove)


################## LDA
model.lda <- lda(formula=Response~., data=train)
predict.lda <- predict(model.lda, dplyr::select(test, -Response))

score <- c()
score$lda <- ScoreQuadraticWeightedKappa(test$Response, predict.lda$class, 1, 8)


################## QDA
model.qda <- qda(formula=Response~., data=train)
predict.qda <- predict(model.qda, dplyr::select(test, -Response))

score$qda <- ScoreQuadraticWeightedKappa(test$Response, predict.qda$class, 1, 8)

print(score) # lda beats qda

################## plot LDA (2 dimensional)

predict.lda.2d <- predict(model.lda, dplyr::select(test, -Response), dimen=2)

dataset <- data.frame(rating = test$Response, lda = predict.lda.2d$x)

p <- ggplot(dataset) + geom_point(aes(x=lda.LD1,y=lda.LD2,colour=rating))
print(p)
