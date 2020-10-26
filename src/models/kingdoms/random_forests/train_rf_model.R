## ----------------------------------------------------------------------------------- ##
## Program to train 3-fold cross-validated Random Forest model (TO-DO: param optimization)
## ----------------------------------------------------------------------------------- ##

library(randomForest)

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different
load("processed_data/1e3_5kingdoms/train_1e3_5kingdoms.RData") #yours will be different

set.seed(123)

trControl <- trainControl(method  = "cv",
                          number  = 5, 
                          summaryFunction = multiClassSummary)

parametersGrid <-  expand.grid(mtry = 1:64)

fit <- train(Kingdom ~ .,
             method     = "rf",
             trControl  = trControl,
             metric     = "Mean_F1",
             data       = myInput_train, 
             tuneGrid   = parametersGrid)
summary(fit)

rf.model <- randomForest(Kingdom ~ ., 
                         data = myInput_train, 
                         mtry = 35)

save(rf.model, file = "models/kingdoms/random_forests/rf_model.RData") #yours will be different
save(fit, file = "models/kingdoms/random_forests/rf_model_CV.RData") #yours will be different
