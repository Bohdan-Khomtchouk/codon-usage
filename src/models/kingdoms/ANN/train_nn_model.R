## ----------------------------------------------------------------------------------- ##
## Program to train a neural nets model using cross-validation
## ----------------------------------------------------------------------------------- ##

library(caret)

rm(list=ls())

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different
load("processed_data/1e3_5kingdoms/train_1e3_5kingdoms.RData") #yours will be different

set.seed(123)

# bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

trControl <- trainControl(method  = "cv",
                          number  = 5, 
                          summaryFunction = multiClassSummary)

parametersGrid <-  expand.grid(size = 1:10,
                               decay = seq(from = 0.1, to = 0.5, by=0.1)
)

fit <- train(Kingdom ~ .,
             method     = "nnet",
             trControl  = trControl,
             metric     = "Mean_F1",
             data       = myInput_train,
             tuneGrid   = parametersGrid
             )

print(fit)
nnet.model <- fit ## Renaming the model

save(nnet.model, file = "models/kingdoms/ANN/nnet_model.RData") #yours will be different
