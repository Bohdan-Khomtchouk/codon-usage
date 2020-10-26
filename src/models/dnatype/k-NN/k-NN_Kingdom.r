myInput <- read.csv("processed_data/processed_1e3_5kingdoms.csv", stringsAsFactors = FALSE) #yours will be different
myInput <- myInput[-3] #remove SpeciesID from ML models #yours will be different

#recode 3 kingdoms (archaea, bacteria, eukaryotes) as factors
# myInput$Kingdom <- factor(myInput$Kingdom, levels = c("arc", "bct", "euk"), labels = c("archaea", "bacteria", "eukaryote"))

#recode 4 kingdoms (archaea, bacteria, eukaryotes, viruses) as factors
# myInput$Kingdom <- factor(myInput$Kingdom, levels = c("arc", "bct", "euk", "vrl"), labels = c("archaea", "bacteria", "eukaryote", "virus"))

#recode 5 kingdoms (archaea, bacteria, eukaryotes, viruses, phages) as factors
myInput$Kingdom <- factor(myInput$Kingdom, levels = c("arc", "bct", "euk", "vrl", "phg"), labels = c("archaea", "bacteria", "eukaryote", "virus", "bacteriophage"))

#inspect
#> table(myInput$Kingdom)

#archaea  bacteria eukaryote 
#65      1288      1036 


#---------------------random sampling for training/test data---------------------
set.seed(123)
# train_sample <- sample(2389, 1911) #80% training split for 3 kingdoms
# train_sample <- sample(9937, 2485) #80% training split for 4 kingdoms
train_sample <- sample(10406, 8324) #80% training split for 5 kingdoms

myInput_train <- myInput[train_sample, ]
myInput_train_labels <- myInput[train_sample, 1] #Kingdom labels only
myInput_train <- subset(myInput_train, select = -c(Kingdom, DNAtype, Ncodons, SpeciesName))

myInput_test  <- myInput[-train_sample, ]
myInput_test_labels <- myInput[-train_sample, 1] #Kingdom labels only
myInput_test <- subset(myInput_test, select = -c(Kingdom, DNAtype, Ncodons, SpeciesName))

#---------------------k-NN---------------------

#install.packages("class")

# 3 kingdoms (arc, bct, euk) [try different values of k]
myInput_test_pred <- class::knn(train = myInput_train, test = myInput_test, cl = myInput_train_labels, k = 1)

# 4 kingdoms (arc, bct, euk, vrl) [try different values of k]
myInput_test_pred <- class::knn(train = myInput_train, test = myInput_test, cl = myInput_train_labels, k = 4)

# 4 kingdoms (arc, bct, euk, vrl, phg) [try different values of k]
myInput_test_pred <- class::knn(train = myInput_train, test = myInput_test, cl = myInput_train_labels, k = 5, prob = TRUE)

prob <- attr(myInput_test_pred, "prob")
#install.packages("gmodels")

gmodels::CrossTable(x = myInput_test_labels, y = myInput_test_pred, prop.chisq = FALSE)

