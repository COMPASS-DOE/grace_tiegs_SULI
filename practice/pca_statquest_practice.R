## Following along with StatQuest for PCA in R for practice
##
##
## Grace Tiegs
## 6-4-25
##
# ################ #
# ################ #

## STEP 1 - DATA ORGANIZATION
## make some practice data
data.matrix <- matrix(nrow=100, ncol=10)
colnames(data.matrix) <- c(
  paste("wt", 1:5, sep=""),
  paste("ko", 1:5, sep=""))

rownames(data.matrix) <- paste("gene", 1:100, setp="")

for (i in 1:100) {
  wt.values <- rpois(5, lambda=sample(x=10:1000, size = 1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  
  data.matrix[i,] <- c(wt.values, ko.values)
}

head(data.matrix)

## call function to do PCA on out data
pca <- prcomp(t(data.matrix), scale=TRUE) 
      # transpose the data since we need the columns and genes variables
plot(pca$x[,1], pca$x[,2])

pca.var <- pca$sdev^2 # stdev squared to calculate variation in og data
pca.var.per <- round(pca.var/sum(pca.var)*100, 1) # make it into a percentage and round

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
# plot then on a bar graph to more easily visualize

### STEP 2 - PLOTTING
# load in library
library(ggplot2)

# format the data the way ggplot2 likes it
pca.data <- data.frame(Sample=rownames(pca$x), # one column with the sample ids
                       # two columns for the X
                       # and Y coordinates for each sample
                       X=pca$x[,1], 
                       Y=pca$x[,2])

# plotting this with a graph
#  x-axis - what percentage of variation in og data pc1 acccounts for
#  y-axis - what percentage of varation in og data pc2 accounts for
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

loading_scores <- pca$rotation[,1] # loading score for 1
gene_scores <- abs(loading_scores) # genes that push to the left will have
                                   # large (-) values, to the right is (+)
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])

top_10_genes

pca$rotation[top_10_genes, 1] # show the scores and +/- sign
