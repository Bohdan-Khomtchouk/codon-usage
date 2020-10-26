## ----------------------------------------------------------------------------------- ##
## Program to create AUC plots for XGBoost model 
## ----------------------------------------------------------------------------------- ##

library(caret)
library(plotROC)
library(data.table)
library(ggplot2)

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different

rm(list=ls())
load(file = "models/dnatype/XGBoost/xgb_model.RData") #yours will be different
load(file = "processed_data/1e3_5kingdoms/test_1e3_3dnatype.RData") #yours will be different

model <- xgb.model
myInput_test_labels <- myInput_test[, 1] #Kingdom labels only
myInput_test_preds <- predict(model, newdata = myInput_test)

create.preds.table <- function(myInput_test){
  test.labels <- data.table(as.factor(myInput_test$DNAtype))
  test.labels <-one_hot(test.labels)
  
  model.preds <- as.data.frame(predict(model, newdata = myInput_test[, -1], type = "prob"))
  
  test.labels <- cbind(test.labels, model.preds)
  colnames(test.labels) <- c("0_true", "1_true", "2_true",  "0_pred", "1_pred", "2_pred")
  
  return(test.labels)
}

table.preds <- create.preds.table(myInput_test)

#Convert this to long format
dna_type_0 <- c("0_true", "0_pred")
dna_type_1 <- c("1_true", "1_pred")
dna_type_2 <- c("2_true", "2_pred")

t_dna_type_0 <- subset(table.preds, select = dna_type_0)
t_dna_type_0$class <- "0" #This column is for plotting AUC curve for the '0' dnatype 
colnames(t_dna_type_0) <- c("true_label", "pred_prob", "DNAtype")

t_dna_type_1 <- subset(table.preds, select = dna_type_1)
t_dna_type_1$class <- "1" 
colnames(t_dna_type_1) <- c("true_label", "pred_prob", "DNAtype")

t_dna_type_2 <- subset(table.preds, select = dna_type_2)
t_dna_type_2$class <- "2" 
colnames(t_dna_type_2) <- c("true_label", "pred_prob", "DNAtype")

#Combine all tables
table.roc <- rbind(t_dna_type_0, t_dna_type_1, t_dna_type_2)

ggplot(table.roc, aes(m = pred_prob, d = true_label, color = DNAtype)) + 
  geom_roc(pointsize = 0.1, labels = FALSE) + 
  xlab("False Positive Rate") + 
  ylab("True Positive Rate")
