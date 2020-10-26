## ----------------------------------------------------------------------------------- ##
## Program to generate PCA plot for the 64-D CUTG data color-coded by Kingdom
## ----------------------------------------------------------------------------------- ##

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different
rm(list = ls())

library(plyr)
library(dplyr)
library(ggfortify)

myInput <- read.csv("models/k-NN/processed_1e3_5kingdoms.csv", stringsAsFactors = FALSE) #yours will be different
myInput.kingdom <- subset(myInput, select = -c(DNAtype, Ncodons, SpeciesName, SpeciesID))
myInput.kingdom$Kingdom <- as.factor(myInput.kingdom$Kingdom)

cu.data <- myInput.kingdom %>% select(-c(Kingdom))
cu.labels <- myInput.kingdom %>% select(Kingdom)

set.seed(123)

cu.pca <- prcomp(cu.data, center = TRUE, scale. = TRUE)
summary(cu.pca)

jpeg("plots/PCA_Kingdom.jpg")
autoplot(cu.pca, data = myInput.kingdom, colour = 'Kingdom')
dev.off()

# table(myInput$Kingdom)
# 
# arc  bct  euk  phg  vrl 
# 114 2556 6914  586 2838 