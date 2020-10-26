## ----------------------------------------------------------------------------------- ##
## Program to create AUC plots for random forest model 
## ----------------------------------------------------------------------------------- ##

library(caret)
library(ggplot2)
library(plotROC)
library(data.table)
library(mltools)

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different

rm(list=ls())
load(file = "models/kingdoms/random_forests/rf_model.RData") #yours will be different
load(file = "processed_data/1e3_5kingdoms/test_1e3_5kingdoms.RData") #yours will be different

model <- rf.model
myInput_test_labels <- myInput_test$Kingdom
myInput_test_preds <- predict(model, newdata = myInput_test)

create.preds.table <- function(myInput_test){
  test.labels <- data.table(as.factor(myInput_test$Kingdom))
  test.labels <-one_hot(test.labels)
  
  model.preds <- as.data.frame(predict(model, newdata = myInput_test[, -1], type = "prob"))
  
  test.labels <- cbind(test.labels, model.preds)
  colnames(test.labels) <- c("archaea_true", "bacteria_true", "eukaryote_true", "virus_true", "bacteriophage_true", "archaea_pred", "bacteria_pred", "eukaryote_pred", "virus_pred", "bacteriophage_pred")
  
  return(test.labels)
}

table.preds <- create.preds.table(myInput_test)

#Convert this to long format
archaea <- c("archaea_true", "archaea_pred")
bacteria <- c("bacteria_true", "bacteria_pred")
eukaryote <- c("eukaryote_true", "eukaryote_pred")
virus <- c("virus_true", "virus_pred")
bacteriophage <- c("bacteriophage_true", "bacteriophage_pred")

t_archaea <- subset(table.preds, select = archaea)
t_archaea$class <- "Archaea"
colnames(t_archaea) <- c("true_label", "pred_prob", "Kingdom")

t_bacteria <- subset(table.preds, select = bacteria)
t_bacteria$class <- "Bacteria"
colnames(t_bacteria) <- c("true_label", "pred_prob", "Kingdom")

t_eukaryote <- subset(table.preds, select = eukaryote)
t_eukaryote$class <- "Eukaryote"
colnames(t_eukaryote) <- c("true_label", "pred_prob", "Kingdom")

t_virus <- subset(table.preds, select = virus)
t_virus$class <- "Virus"
colnames(t_virus) <- c("true_label", "pred_prob", "Kingdom")

t_bacteriophage <- subset(table.preds, select = bacteriophage)
t_bacteriophage$class <- "Bacteriophage"
colnames(t_bacteriophage) <- c("true_label", "pred_prob", "Kingdom")

#Combine all tables
table.roc <- rbind(t_archaea, t_bacteria, t_eukaryote, t_virus, t_bacteriophage)

ggplot(table.roc, aes(m = pred_prob, d = true_label, color = Kingdom)) + geom_roc(labels = FALSE, pointsize = 0.1) + xlab("False Positive Rate") + ylab("True Positive Rate")
