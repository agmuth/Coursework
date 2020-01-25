library(dplyr)
library(caret)
library(purrr)
library(data.table)
library(Metrics)

#classifies categorical variables as factors
add_categorical <- function(x){
  vars <- c( "Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", 
             "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", 
             "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", 
             "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", 
             "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", 
             "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", 
             "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", 
             "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", 
             "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", 
             "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")
  
  x <- mutate_at(x, vars, as.factor)
  
  return(x)
}

#classifies continuous variables as numerics
add_continuous <- function(x){
  vars <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", "Employment_Info_6", "Insurance_History_5", 
            "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5")
 
   x <- mutate_at(x, vars, as.numeric)
  
  return(x)
}

#classifies discrete variables as integers (0-1 encoding)
add_discrete <- function(x){
  vars <- c("Medical_History_1", "Medical_History_10", "Medical_History_15", "Medical_History_24", "Medical_History_32")
  
  x <- mutate_at(x, vars, as.integer)
  x <- mutate_at(x, vars(starts_with("Medical_Keyword")), as.integer)
  
  return(x)
  
}

#cleans data and adds variable transforms 
clean <- function(x){
  
  vars_cat <- c( "Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", 
                 "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", 
                 "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", 
                 "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", 
                 "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", 
                 "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", 
                 "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", 
                 "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", 
                 "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", 
                 "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")
  
  vars_cts <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", "Employment_Info_6", "Insurance_History_5", 
                "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5")
  
  if(!("Response" %in% colnames(x))){
    #data is testing data with no Response column
    #add Response column temporarily to use dummyVars and model.matrix
    x$Response <- 0 
  }
  
  x <- x %>% add_continuous(.) %>% add_discrete(.) %>% mutate_at(., vars(Response), as.factor)
  x$num_NA <- rowSums(is.na(x)) %>% as.numeric(.) # counter for number of missng fields
  Id <- x$Id
  x <- x[,c(ncol(x)-1, ncol(x), 2:(ncol(x)-2))]
  
  dummies <- dummyVars(Response~., data=x[,colnames(x) %in% c("Response", "Product_Info_2")])
  x <- cbind.data.frame(x[,!(colnames(x) %in% c("Product_Info_2"))], predict(dummies, newdata=x[,colnames(x) %in% c("Response", "Product_Info_2")]))
  x <- x %>% setNames(make.names(names(.), unique=TRUE))
  
  #replace missing continuous variables with sample median (other methods took too long)
  #should be fine for both train and test sets as they are large enough that the medians are likely to be very similar
  preproc <- preProcess(x, method="medianImpute") 
  x <- predict(preproc, x)
  
  #add interaction terms for similar categories of continuous variables
  
  x <- cbind.data.frame(Id=Id, x[, !(colnames(x) %in% c(c("Ins_Age", "Ht", "Wt", "BMI"), #health factors
                                      c("Employment_Info_1", "Employment_Info_4", "Employment_Info_6"), #employment factors 
                                      c("Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5"), #family history factors
                                      c("Product_Info_4", "Insurance_History_5")))], 
                        model.matrix(Response~.^2 -1, data=x[, colnames(x) %in% c("Response", "Ins_Age", "Ht", "Wt", "BMI")]),
                        model.matrix(Response~.^2 -1, data=x[, colnames(x) %in% c("Response", "Employment_Info_1", "Employment_Info_4", "Employment_Info_6")]),
                        model.matrix(Response~.^2 -1, data=x[, colnames(x) %in% c("Response", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5")]),
                        model.matrix(Response~.^2 -1, data=x[, colnames(x) %in% c("Response", "Product_Info_4", "Insurance_History_5")]))
  
  
  
  if(sum(x$Response != 0) == 0){
    #data is test set remove response column
    x <- dplyr::select(x, -Response)
  }
  colnames(x) <- make.names(colnames(x))
  return(x) 
}


#' Custom SQWK function to be used to track xgboost model with objective multi:softprob progress 
#' as it is fit with the mlr package with the defualt predictions set to probabilities. 
#' In this case the mlr package passes in predictions (here preds) as a single vector of length num_classes x num_obvs. 
#' Through trial and error it appears that for any integer i sum(preds[i*numclasses:(i+1)*num_classes]) 
#' sums to one and thus belong to the same observation. It is assumed in addition that the 
#' observations are passed in in order and that probabilities are in order of class labels
#' (seems reasonable). Ultimately, this is just a function for tracking the boosting progression 
#' and the final model is chosen using SQWK_mlr.
#' 
#' This function is implemented internally and not meant to be called directly.
#'
#' @param preds vector of predictions of length num_classes x num_obvs
#' @param dtrain xgbDMatrix object
#'
#' @return quadratic weighted kappa
#' @export
#'
#' @examples
SQWK_xgb <- function (preds, dtrain){
  preds <- t(matrix(preds, nrow=8))  #reshape preds
  #preds <- apply(preds[,-1], 1, which.max) - 1 #classify according to maximum probability 
  preds <- apply(preds, 1, which.max) - 1 #classify according to maximum probability 
  
  rater.b <- preds
  rater.a <- getinfo(dtrain, "label")
  min.rating <- 0
  max.rating <- 7
  error <- Metrics::ScoreQuadraticWeightedKappa(rater.a, rater.b, min.rating=1, max.rating=7)
  return(list(metric="Quadratic Weighted Kappa", value=error))
}

#SQWK_xgb() edited for 1-vs-all binary classification models.
SQWK_xgb_binary <- function (preds, dtrain){
  rater.b <- factor(as.integer(preds >= 0.5))
  rater.a <- getinfo(dtrain, "label")
  min.rating <- 0
  max.rating <- 1
  error <- Metrics::ScoreQuadraticWeightedKappa(rater.a, rater.b, min.rating=0, max.rating=1)
  return(list(metric="Quadratic Weighted Kappa", value=error))
}

#Writes submissing to .csv file as per competition specifications.
write_submission <- function(Id, Response, file){
  fwrite(cbind.data.frame(Id, Response), file)
}


#' Score Quadratic Weighted Kappa implementation for mlr models trained to predict all eight classes. 
#' 
#'This function is implemented internally and not meant to be called directly. 
#'
#' @param pred: a mlr predictino object
#'
#' @return quadratic weighted kappa
SQWK_mlr <- function(pred){
  rater.a <- pred$data$truth
  rater.b <- pred$data$response
  error <- Metrics::ScoreQuadraticWeightedKappa(rater.a, rater.b, 1, 8)
  return(error)
}

#SQWK_mlr() edited for 1-vs-all binary classification models.
SQWK_mlr_binary <- function(pred){
  rater.a <- pred$data$truth
  rater.b <- pred$data$response
  error <- Metrics::ScoreQuadraticWeightedKappa(rater.a, rater.b, 0, 1)
  return(error)
}

#' Assigns each observation the weight sum(negative instances) / sum(positive instances) so that each class has
#' the same total weight in the dataset.
#'
#' @param x: response vector of class labels.
#'
#' @return vector of observation weights for training.
get_weights <- function(x){
  counts <- summary(x)
  case_weights <- (sum(counts) - counts) / counts
  weights <- case_weights[names(case_weights)[x]]
  return(weights)
}

#' Projects data onto subspace defined by LDA.
#'
#' @param x a data frame. Assumed to have ID column removed and response in first column
#' @param lda_model lda model used to project the data onto the subspace.
#' 
#' @return projected data
lda_projection <- function(x, lda_model){
  x <- as.data.frame(x)
  col_remove <- removeCorrelatedFeatures(x)
  x <- dplyr::select(x, -col_remove)
  proj <- predict(lda_model, x[,-1])$x
  x <- cbind.data.frame(Response=x$Response, proj)
  return(x)
}


# Takes the training data set and outputs the features to remove (correlated)
# input: 
#       train, a dataframe with Response and all the explanatory variables
#       perc, percentage of values which must be different from mode
#       cutoff, variable is removed if absolute pairwise corr > cutoff
#
#       Needs mlr, dplyr, caret packages
# output: 
#       vector of column names to drop

removeCorrelatedFeatures <- function(train, perc=0.005, cutoff=0.85) {
  
  require(mlr)
  require(dplyr)
  require(caret)
  
  col_remove <- c()
  
  # remove constant features
  train_small <- removeConstantFeatures(train, dont.rm="Response", perc=perc) 
  
  # remove correlated features
  col_remove <- as.vector(findCorrelation(cor(dplyr::select(train_small, -Response)), 
                                          cutoff=cutoff, names=TRUE, verbose=TRUE))
  train_small <- dplyr::select(train_small, -col_remove)
  
  # loop through each class and remove constant and correlated features within that class
  for(i in 1:8) {
    # remove constant features
    print(i)
    train_amend <- removeConstantFeatures(train_small[train_small$Response==i, ], dont.rm="Response", perc=perc)
    col_remove <- setdiff(colnames(train_small), colnames(train_amend))
    train_small <- dplyr::select(train_small, -col_remove)
    
    # remove correlated features
    col_remove <- as.vector(findCorrelation(cor(train_small[train_small$Response==i, -1]), 
                                            cutoff=cutoff, names=TRUE, verbose=TRUE))
    train_small <- dplyr::select(train_small, -col_remove)
    
  }
  
  return(setdiff(colnames(train), colnames(train_small)))
  
}


#' Custom predict function to implement prediction using the 8 1-vs-all classification models.
#'
#' @param models list of 1 vs all models fit using mlr package. ith model should predict ith class.
#' @param newdata new data to predict on 
#' @param type one of "response" or "prob"
#'
#' @return
#' @export
#'
#' @examples
custom_predict <- function(models, newdata, type="response"){
  
  preds <- lapply(models, function(m){return(predict(m, newdata=newdata, type="prob")$data[,3])})
  cnames <- unlist(lapply(1:length(models), function(i){paste0("Class", i)}))
  
  preds <- as.data.frame(preds, col.names=cnames)
  preds <- as.data.frame(t(apply(preds, 1, softmax)))
  
  if(type == "response"){
    return(as.factor(apply(preds, 1, which.max)))
  }else if(type == "prob"){
    return(preds)
  }else{
    stop("Argument type must be one of 'response' or 'prob'")
  }
  
}

#softmax function
softmax <- function(p){
  return(exp(p) / sum(exp(p)))
}
