## ----------------------------------------------------------------------------------- ##
## Program to split data into train and test sets
## ----------------------------------------------------------------------------------- ##

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different

library(plyr)
library(dplyr)
library(caret)

input_file <- "processed_data/1e3_5kingdoms/processed_1e3_5kingdoms.csv" #yours will be different

#Load input
myInput <- read.csv(input_file, stringsAsFactors = FALSE) #yours will be different
myInput <- myInput[-3] #remove SpeciesID from ML models #yours will be different
myInput$DNAtype <- as.character(myInput$DNAtype)

#Only include DNAtype classes occuring > 100 times.
myInput <- myInput %>% 
  group_by(DNAtype) %>%
  filter(n() > 100) %>%
  ungroup()
  
myInput$DNAtype <- factor(myInput$DNAtype, levels = c("0", "1", "2"), labels = c("0", "1", "2"))

#inspect
#> table(myInput$Kingdom)

#archaea  bacteria eukaryote 
#65      1288      1036 


#---------------------random sampling for training/test data---------------------
set.seed(123)
# train_sample <- sample(2389, 1911) #80% training split for 3 kingdoms
# train_sample <- sample(9937, 2485) #80% training split for 4 kingdoms
train_sample <- sample(nrow(myInput), round(nrow(myInput) * 0.8)) #80% training split for 3 dnatypes

#Preparing train data
myInput_train <- myInput[train_sample, ]
myInput_train_labels <- myInput[train_sample, 1] #Kingdom labels only
myInput_train <- subset(myInput_train, select = -c(Kingdom, Ncodons, SpeciesName))

#Preparing test data
myInput_test  <- myInput[-train_sample, ]
myInput_test <- subset(myInput_test, select = -c(Kingdom, Ncodons, SpeciesName))

save(myInput_train, file = "processed_data/1e3_5kingdoms/train_1e3_3dnatype.RData") #yours will be different
save(myInput_test, file = "processed_data/1e3_5kingdoms/test_1e3_3dnatype.RData") #yours will be different
