setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different

myInput <- read.csv("wolfgang_original_CUTG_datafiles/processed_1e3.csv", stringsAsFactors = FALSE) #yours will be different
myInput <- myInput[-3] #remove SpeciesID from ML models #yours will be different

#inspect
table(myInput$Kingdom)
table(myInput$DNAtype)

#recode as factors
myInput$Kingdom <- factor(myInput$Kingdom, levels = c("archaea", "bacteria", "invertebrate", "mammal", "phage", "plasmid", "plant", "primate", "rodent", "virus", "vertebrate"))
myInput$DNAtype <- factor(myInput$DNAtype, levels = c("nuclear", "mitochondrion", "chloroplast"))

#see percentages
round(prop.table(table(myInput$Kingdom)) * 100, digits = 1)
round(prop.table(table(myInput$DNAtype)) * 100, digits = 1)

#chk it
str(myInput)

#---------------------random sampling for training/test data---------------------
set.seed(123)
train_sample <- sample(12980, 10384) #80% training split

myInput_train <- myInput[train_sample, ]
myInput_train_labels <- myInput[train_sample, 2] #DNAtype labels only
myInput_train <- subset(myInput_train, select = -c(Kingdom, DNAtype, Ncodons, SpeciesName))

myInput_test  <- myInput[-train_sample, ]
myInput_test_labels <- myInput[-train_sample, 2] #DNAtype labels only
myInput_test <- subset(myInput_test, select = -c(Kingdom, DNAtype, Ncodons, SpeciesName))


#---------------------k-NN---------------------

#install.packages("class")

myInput_test_pred <- class::knn(train = myInput_train, test = myInput_test, cl = myInput_train_labels, k = 3)

# install.packages("gmodels")
gmodels::CrossTable(x = myInput_test_labels, y = myInput_test_pred, prop.chisq = FALSE)
