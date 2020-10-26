## ----------------------------------------------------------------------------------- ##
## Program to generate t-SNE 2-D plot for the 64-D CUTG data 
## ----------------------------------------------------------------------------------- ##

setwd("~/dev/cuAI/CUTG_ML_paper_datasets/") #yours will be different

library(Rtsne)
library(umap)

myInput <- read.csv("models/k-NN/processed_1e3_5kingdoms.csv", stringsAsFactors = FALSE) #yours will be different
myInput_train_tsne <- subset(myInput, select = -c(DNAtype, Ncodons, SpeciesName, SpeciesID))
myInput_train_tsne$Kingdom <- factor(myInput_train_tsne$Kingdom, levels = c("arc", "bct", "euk", "vrl", "phg"), labels = c("archaea", "bacteria", "eukaryote", "virus", "bacteriophage"))
trn <- data.matrix(myInput_train_tsne)

set.seed(123)

# perform dimensionality redcution from 64D to 2D
tsne <- Rtsne(as.matrix(trn[, 2:65]), check_duplicates = FALSE, pca = FALSE, perplexity=50, theta=0.5, dims=3, max_iter = 5000)
kingdoms.umap <- umap(trn[, 2:65])


# display the results of t-SNE
cols = rainbow(5)
jpeg("plots/t-SNE.jpg")
plot(tsne$Y, col=trn[,1], bg=trn[,1], pch=16, cex=0.4)

legend("topright", legend = c('archaea','bacteria','eukaryote', 'virus', 'bacteriophage'), pch = 16, col = rainbow(5), cex=0.6)
dev.off()

#CONCLUSIONS:
#There doesn't seem to be any clear separation between the kingdoms