## ----------------------------------------------------------------------------------- ##
## Program to generate UMAP 2-D plot for the 64-D CUTG data color-coded by Kingdom
## ----------------------------------------------------------------------------------- ##

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different
rm(list = ls())

library(plyr)
library(dplyr)
library(ggplot2)
library(umap)

# myInput <- read.csv("processed_data/1e3_5kingdoms/train_1e3_3dnatype.RData", stringsAsFactors = FALSE)
load("processed_data/1e3_5kingdoms/train_1e3_5kingdoms.RData") #yours will be different

cu.data <- myInput_train %>% select(-c(Kingdom))
cu.labels <- myInput_train %>% select(Kingdom)

set.seed(123)

#Apply umap transformation
cu.umap <- umap(cu.data)
print(cu.umap)

umap.coords <- cu.umap$layout
colnames(umap.coords) <- c("UMAP1", "UMAP2")
cu.data <- cbind(cu.data, umap.coords)

# table(myInput$Kingdom)
# 
# arc  bct  euk  phg  vrl 
# 114 2556 6914  586 2838 

jpeg("plots/UMAP_Kingdom.jpg")

#Visualize the dataset using the UMAP coordinates 
cu.data %>% 
  mutate(Kingdom = cu.labels$Kingdom) %>%
  ggplot(aes(UMAP1, UMAP2, color = Kingdom)) + geom_point()

dev.off()