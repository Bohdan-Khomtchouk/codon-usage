## ----------------------------------------------------------------------------------- ##
## Program to train k-NN model using cross-validation
## ----------------------------------------------------------------------------------- ##

library(caret)

rm(list=ls())

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different
load("processed_data/1e3_5kingdoms/train_1e3_5kingdoms.RData") #yours will be different

set.seed(123)

trControl <- trainControl(method  = "cv",
                          number  = 5, 
                          summaryFunction = multiClassSummary)

parametersGrid <-  expand.grid(k = 1:5)

fit <- train(Kingdom ~ .,
             method     = "knn",
             trControl  = trControl,
             metric     = "Mean_F1",
             data       = myInput_train, 
             tuneGrid   = parametersGrid
             )

print(fit)
knn.model <- fit ## Renaming the model

save(knn.model, file = "models/kingdoms/k-NN/knn_model.RData") #yours will be different
