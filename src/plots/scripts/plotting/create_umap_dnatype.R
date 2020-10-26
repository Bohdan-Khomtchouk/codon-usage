## ----------------------------------------------------------------------------------- ##
## Program to generate UMAP 2-D plot for the 64-D CUTG data color-coded by DNAType
## ----------------------------------------------------------------------------------- ##

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different

library(plyr)
library(dplyr)
library(ggplot2)
library(umap)

load("processed_data/1e3_5kingdoms/train_1e3_3dnatype.RData") #yours will be different

cu.data <- myInput_train %>% select(-c(DNAtype))
cu.labels <- myInput_train %>% select(DNAtype)

set.seed(123)

#Apply umap transformation
cu.umap <- umap(cu.data)
print(cu.umap)

umap.coords <- cu.umap$layout
colnames(umap.coords) <- c("UMAP1", "UMAP2")
cu.data <- cbind(cu.data, umap.coords)

# table(myInput_train_tsne$DNAtype)
# 
# 0    1    2    3    4    5    6    7    9   11   12 
# 9247 2899  816    2   31    2    1    1    2    2    5 

jpeg("plots/UMAP_DNAType.jpg")

#Visualize the dataset using the UMAP coordinates 
cu.data %>% 
  mutate(DNAtype = cu.labels$DNAtype) %>%
  ggplot(aes(UMAP1, UMAP2, color = DNAtype)) + geom_point()

dev.off()

#CONCLUSIONS:
#The DNATypes appear to be separated