## ----------------------------------------------------------------------------------- ##
## Program to train XGBoost model using cross-validation
## ----------------------------------------------------------------------------------- ##

library(caret)

rm(list=ls())

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different
load("processed_data/1e3_5kingdoms/train_1e3_3dnatype.RData") #yours will be different

set.seed(123)

# bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

trControl <- trainControl(method  = "cv",
                          number  = 5, 
                          summaryFunction = multiClassSummary)

parametersGrid <-  expand.grid(eta = 0.1, 
                               colsample_bytree=c(0.5,0.7),
                               max_depth=c(3,6),
                               nrounds=100,
                               gamma=1,
                               min_child_weight=2, 
                               subsample=0.5
)

fit <- train(DNAtype ~ .,
             method     = "xgbTree",
             trControl  = trControl,
             metric     = "Mean_F1",
             data       = myInput_train, 
             tuneGrid   = parametersGrid
             )

print(fit)
xgb.model <- fit ## Renaming the model

save(xgb.model, file = "models/dnatype/XGBoost/xgb_model.RData") #yours will be different
