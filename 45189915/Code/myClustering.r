# Xin Chen 45189915

# code to complete Task 2

library("ggplot2") # Data visualization


# 2.1 load the preprocessed data file from task 1 into a data frame
#     exclude Age, Gender, AG_Ratio and Class attributes in this task
ILPD_task2 = readRDS("./Data/ilpd_processed.Rda")
ILPD_withoutclass = ILPD_task2[,!(names(ILPD_task2) %in% c("Age","Gender","AG_Ratio","Class"))]


# 2.2 rescale the values of every column to the range of (0,1)
#     reference link: https://stackoverflow.com/a/15468888/6350054
ILPD_scaled_withoutclass = apply(ILPD_withoutclass, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
ILPD_scaled_withoutclass = as.data.frame(ILPD_scaled_withoutclass)


# 2.3 cluster the data into 2 clusters using K-means clustering (using default parameters for the kmeans function)
#     plot the results of the clusters as a 2D plot (where x-axis is Alkphos and the y-axis is TP)
set.seed(45)
clusters_2 <- kmeans(ILPD_scaled_withoutclass, 2, nstart = 20)
clusters_2_num <- as.factor(clusters_2$cluster)
plot_q3 <- ggplot(ILPD_scaled_withoutclass, aes(Alkphos, TP, color=clusters_2_num)) + geom_point()
png(filename="Plot/task2.3.png")
plot(plot_q3)
dev.off()


# 2.4 plot another 2D plot with the same dimensions above, but color the points according to the "Class" column
ILPD_scaled_withclass = ILPD_scaled_withoutclass
ILPD_scaled_withclass$Class <- ILPD_task2$Class
plot_q4 <- ggplot(ILPD_scaled_withclass, aes(Alkphos, TP, color = Class)) + geom_point()
png(filename="Plot/task2.4.png")
plot(plot_q4)
dev.off()


# 2.5 compare the 2 plots in 2.2 and 2.3
#     do the clusters visually represent the patent vs non_patient classes?


# 2.6 cluster the data into more than 2 clusters (i.e., k = 3,4,5) using K-Means clustering
#     plot all the clustering results

# k = 3
set.seed(45)
clusters_3 <- kmeans(ILPD_scaled_withoutclass, 3, nstart = 20)
clusters_3_num <- as.factor(clusters_3$cluster)
plot_q6_1 <- ggplot(ILPD_scaled_withoutclass, aes(Alkphos, TP, color=clusters_3_num)) + geom_point()
png(filename="Plot/task2.6-k=3.png")
plot(plot_q6_1)
dev.off()

# k = 4
set.seed(45)
clusters_4 <- kmeans(ILPD_scaled_withoutclass, 4, nstart = 20)
clusters_4_num <- as.factor(clusters_4$cluster)
plot_q6_2 <- ggplot(ILPD_scaled_withoutclass, aes(Alkphos, TP, color=clusters_4_num)) + geom_point()
png(filename="Plot/task2.6-k=4.png")
plot(plot_q6_2)
dev.off()

# k = 5
set.seed(45)
clusters_5 <- kmeans(ILPD_scaled_withoutclass, 5, nstart = 20)
clusters_5_num <- as.factor(clusters_5$cluster)
plot_q6_3 <- ggplot(ILPD_scaled_withoutclass, aes(Alkphos, TP, color=clusters_5_num)) + geom_point()
png(filename="Plot/task2.6-k=5.png")
plot(plot_q6_3)
dev.off()


# 2.7 compare the plots and sum of squared error (SSE) obtained in the previous task and provide your comments on the quality of clustering
# SSE for k-means clustering with k = 2 
SSE_2 = clusters_2$tot.withinss
# SSE for k-means clustering with k = 3 
SSE_3 = clusters_3$tot.withinss
# SSE for k-means clustering with k = 4
SSE_4 = clusters_4$tot.withinss
# SSE for k-means clustering with k = 5
SSE_5 = clusters_5$tot.withinss

# Plot SSE with different k values
sse=vector('numeric')
for(i in 2:15){
  sse[i-1]=sum(kmeans(ILPD_scaled_withoutclass,centers=i)$withinss)
}
sse=as.data.frame(sse)
sse$k=seq.int(2,15)
plot(sse$k,sse$sse,type="b")
dev.off()


# 2.8 apply hierarchical clustering to the data using the hclust function with default parameters
#     plot the corresponding dendrogram
#     particularly, cluster the dendrogram into 2,3,4 and 5 clusters and plot all of them

# using hierarchical clustering method
set.seed(45)
idx <- sample(1:nrow(ILPD_scaled_withclass), 100)
distance_matrix <- dist(as.matrix(ILPD_scaled_withclass[idx, -8]), method = "euclidean")
hc <- hclust(distance_matrix)
# plot the dendrogram
plot(hc, hang = -1, labels = ILPD_scaled_withclass$Class[idx], cex = 0.6)
dev.copy(png,filename="Plot/task2.8.png")
dev.off ()
# cluster the dendrogram into 2 clusters
nclust <- 2
rect.hclust(hc, k=nclust)
dev.copy(png,filename="Plot/task2.8-k=2.png")
dev.off ()
# confusion matrix for 2 clusters
clusterCut <- cutree(hc, 2)
table(ILPD_scaled_withclass$Class[idx], clusterCut)

# cluster the dendrogram into 3 clusters
plot(hc, hang = -1, labels = ILPD_scaled_withclass$Class[idx])
nclust <- 3
rect.hclust(hc, k=nclust)
dev.copy(png,filename="Plot/task2.8-k=3.png")
dev.off ()
# confusion matrix for 3 clusters
clusterCut <- cutree(hc, 3)
table(ILPD_scaled_withclass$Class[idx], clusterCut)

# cluster the dendrogram into 4 clusters
plot(hc, hang = -1, labels = ILPD_scaled_withclass$Class[idx])
nclust <- 4
rect.hclust(hc, k=nclust)
dev.copy(png,filename="Plot/task2.8-k=4.png")
dev.off ()
# confusion matrix for 4 clusters
clusterCut <- cutree(hc, 4)
table(ILPD_scaled_withclass$Class[idx], clusterCut)

# cluster the dendrogram into 5 clusters
plot(hc, hang = -1, labels = ILPD_scaled_withclass$Class[idx])
nclust <- 5
rect.hclust(hc, k=nclust)
dev.copy(png,filename="Plot/task2.8-k=5.png")
dev.off ()
# confusion matrix for 5 clusters
clusterCut <- cutree(hc, 5)
table(ILPD_scaled_withclass$Class[idx], clusterCut)


# 2.9 compare the plots obtained in tasks 2.3, 2.4, 2.6 and 2.8 and provide your observations on the achieved clusters - should we have a new subtype of diseases?


# 2.10 try different agglomeration methods in hierarchical clustering (i.e., "min", "max", "average")
#     plot the resulting dendrograms and provide your comments on the quality of clustering - is the data sensitive to the used agglomeration method?
#     based on your results, what do you think is the default agglomeration method used in task 2.8?

# with "min" method
hc <- hclust(distance_matrix, method = "single")
plot(hc, hang = -1, labels = ILPD_scaled_withclass$Class[idx], cex = 0.6)
dev.copy(png,filename="Plot/task2.10-min.png")
dev.off ()

# with "max" method
hc <- hclust(distance_matrix, method = "complete")
plot(hc, hang = -1, labels = ILPD_scaled_withclass$Class[idx], cex = 0.6)
dev.copy(png,filename="Plot/task2.10-max.png")
dev.off ()

# with average method
hc <- hclust(distance_matrix, method = "average")
plot(hc, hang = -1, labels = ILPD_scaled_withclass$Class[idx], cex = 0.6)
dev.copy(png,filename="Plot/task2.10-average.png")
dev.off ()