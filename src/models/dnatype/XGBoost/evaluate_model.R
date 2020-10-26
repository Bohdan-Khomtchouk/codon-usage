## ----------------------------------------------------------------------------------- ##
## Program to evaluate XGB models using test data
## ----------------------------------------------------------------------------------- ##

library(caret)
library(mltools)
library(data.table)

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different

load(file = "models/dnatype/XGBoost/xgb_model.RData") #yours will be different
load(file = "processed_data/1e3_5kingdoms/test_1e3_3dnatype.RData") #yours will be different

model <- xgb.model
myInput_test_labels <- myInput_test$DNAtype
myInput_test_preds <- predict(model, newdata = myInput_test)

cm <- vector("list", length(levels(myInput_test_labels)))
for (i in seq_along(cm)) {
  positive.class <- levels(myInput_test_labels)[i]
  # in the i-th iteration, use the i-th class as the positive class
  cm[[i]] <- confusionMatrix(myInput_test_preds, myInput_test_labels, 
                             positive = positive.class)
}
metrics <- c("Precision", "Recall")
# print(cm[[1]]$byClass[, metrics])

get.conf.stats <- function(cm) {
  out <- vector("list", length(cm))
  for (i in seq_along(cm)) {
    x <- cm[[i]]
    tp <- x$table[x$positive, x$positive] 
    fp <- sum(x$table[x$positive, colnames(x$table) != x$positive])
    fn <- sum(x$table[colnames(x$table) != x$positive, x$positive])
    # TNs are not well-defined for one-vs-all approach
    elem <- c(tp = tp, fp = fp, fn = fn)
    out[[i]] <- elem
  }
  df <- do.call(rbind, out)
  rownames(df) <- unlist(lapply(cm, function(x) x$positive))
  return(as.data.frame(df))
}

get.precision <- function(cm){
  cm.summary <- get.conf.stats(cm)
  tp <- sum(cm.summary$tp)
  fn <- sum(cm.summary$fn)
  fp <- sum(cm.summary$fp)
  pr <- tp / (tp + fp)
  return(pr)
}
pr <- get.precision(cm)

get.recall <- function(cm){
  cm.summary <- get.conf.stats(cm)
  tp <- sum(cm.summary$tp)
  fn <- sum(cm.summary$fn)
  fp <- sum(cm.summary$fp)
  re <- tp / (tp + fn)
  return(re)
}
re <- get.recall(cm)

get.micro.f1 <- function(cm) {
  cm.summary <- get.conf.stats(cm)
  tp <- sum(cm.summary$tp)
  fn <- sum(cm.summary$fn)
  fp <- sum(cm.summary$fp)
  pr <- tp / (tp + fp)
  re <- tp / (tp + fn)
  f1 <- 2 * ((pr * re) / (pr + re))
  return(f1)
}
micro.f1 <- get.micro.f1(cm)
# print(paste0("Micro F1 is: ", round(micro.f1, 5)))

##### Macro F1
get.macro.f1 <- function(cm) {
  c <- cm[[1]]$byClass # a single matrix is sufficient
  re <- sum(c[, "Recall"]) / nrow(c)
  pr <- sum(c[, "Precision"]) / nrow(c)
  f1 <- 2 * ((re * pr) / (re + pr))
  return(f1)
}
macro.f1 <- get.macro.f1(cm)



######## Accuracy
calculate.accuracy <- function(predictions, ref.labels) {
  return(length(which(predictions == ref.labels)) / length(ref.labels))
}
calculate.w.accuracy <- function(predictions, ref.labels, weights) {
  lvls <- levels(ref.labels)
  if (length(weights) != length(lvls)) {
    stop("Number of weights should agree with the number of classes.")
  }
  if (sum(weights) != 1) {
    stop("Weights do not sum to 1")
  }
  accs <- lapply(lvls, function(x) {
    idx <- which(ref.labels == x)
    return(calculate.accuracy(predictions[idx], ref.labels[idx]))
  })
  acc <- mean(unlist(accs))
  return(acc)
}
acc <- calculate.accuracy(myInput_test_preds, myInput_test_labels)
print(paste0("Accuracy is: ", round(acc, 2)))

calculate.AUC <- function(myInput_test){
  test.labels <- data.table(as.factor(myInput_test$DNAtype))
  test.labels <-one_hot(test.labels)
  
  model.preds <- as.data.frame(predict(model, newdata = myInput_test[, -1], type = "prob"))
  
  test.labels <- cbind(test.labels, model.preds)
  colnames(test.labels) <- c("0_true", "1_true", "2_true", "0_pred_xgb", "1_pred_xgb", "2_pred_xgb")
  
  model.roc <- multi_roc(test.labels)
  return(round(model.roc$AUC$xgb$micro, 4))
}

auc <- calculate.AUC(myInput_test)

round_digits <- 4
print(cm[[1]]$byClass[, metrics])
print(paste0("Micro F1 is: ", round(micro.f1, round_digits)))
print(paste0("Macro F1 is: ", round(macro.f1, round_digits)))
print(paste0("Precision is: ", round(pr, round_digits)))
print(paste0("Recall is: ", round(re, round_digits)))
print(paste0("Accuracy is: ", round(acc, round_digits)))
print(paste0("Model AUC is: ", round(auc, round_digits)))
