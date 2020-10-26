## ----------------------------------------------------------------------------------- ##
## Program to generate PCA plot for the 64-D CUTG data color-coded by DNA type
## ----------------------------------------------------------------------------------- ##

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different
rm(list = ls())

library(plyr)
library(dplyr)
library(ggfortify)

myInput <- read.csv("models/k-NN/processed_1e3_5kingdoms.csv", stringsAsFactors = FALSE) #yours will be different
myInput.DNAtype <- subset(myInput, select = -c(Kingdom, Ncodons, SpeciesName, SpeciesID))
myInput.DNAtype$DNAtype <- as.factor(myInput.DNAtype$DNAtype)

cu.data <- myInput.DNAtype %>% select(-c(DNAtype))
cu.labels <- myInput.DNAtype %>% select(DNAtype)

set.seed(123)

cu.pca <- prcomp(cu.data, center = TRUE, scale. = TRUE)
summary(cu.pca)

jpeg("plots/PCA_DNAtype.jpg")
autoplot(cu.pca, data = myInput.DNAtype, colour = 'DNAtype')
dev.off()

# table(myInput$Kingdom)
# 
# arc  bct  euk  phg  vrl 
# 114 2556 6914  586 2838 