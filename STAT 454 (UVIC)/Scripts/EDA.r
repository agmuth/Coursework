library(ggplot2)
library(GGally)
library(tableone)

setwd("~/Undergraduate Work/STAT 454/Project")
source("codeBank.r")

train <- read.csv("train.csv") 

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

vars_dis <- colnames(train)[!(colnames(train) %in% c(vars_cat, vars_cts, "Id", "Response"))]

train <- clean(train)

#had to do continuous variables in batches for interpretability

ggpairs(train[colnames(train) %in% c("Ins_Age", "Ht", "Wt", "BMI", "Response")], aes(colour=Response, alpha=0.4))
ggpairs(train[colnames(train) %in% c("Employment_Info_1", "Employment_Info_4", "Employment_Info_6", "Response")], aes(colour=Response, alpha=0.4))
ggpairs(train[colnames(train) %in% c("Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5", "Response")], aes(colour=Response, alpha=0.4))
ggpairs(train[colnames(train) %in% c("Product_Info_4", "Insurance_History_5", "num_NA", "Response")], aes(colour=Response, alpha=0.4))

#tableone of 0-1 data preforms Pearsons Chi-square test of independence for each variable with respect to response category
tbl_cts <- CreateTableOne(vars_cts, c("Response"), train)
tbl_dis <-  CreateTableOne(vars_dis, c("Response"), train)
