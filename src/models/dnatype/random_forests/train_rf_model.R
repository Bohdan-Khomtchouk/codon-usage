## ----------------------------------------------------------------------------------- ##
## Program to train 3-fold cross-validated Random Forest model (TO-DO: param optimization)
## ----------------------------------------------------------------------------------- ##

library(plyr)
library(dplyr)
library(caret)
library(randomForest)

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different
load("processed_data/1e3_5kingdoms/train_1e3_3dnatype.RData") #yours will be different

set.seed(123)
trControl <- trainControl(method  = "cv",
                          number  = 5, 
                          summaryFunction = multiClassSummary)

parametersGrid <-  expand.grid(mtry = 1:64)

fit <- train(DNAtype ~ .,
             method     = "rf",
             trControl  = trControl,
             metric     = "Mean_F1",
             tuneGrid   = parametersGrid,
             data       = myInput_train)
summary(fit)

#Best 

rf.model <- randomForest(DNAtype ~ ., 
                         data = myInput_train, 
                         mtry = 10)

save(rf.model, file = "models/dnatype/random_forests/rf_model.RData") #yours will be different
save(fit, file = "models/dnatype/random_forests/rf_model_CV.RData") #yours will be different
