df <- scale(algmtc[1:3606,2:22])
df <- scale(algmirex[1:433,2:22])

df[is.na(df)] <-0
# df[df > 100] <- 0
df <- df[,apply(df ,2, var,na.rm=TRUE) != 0]

df <-data.frame(df)

# Determine number of clusters
wss <- (nrow(df)-1)*sum(apply(df,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(df, 5) # 5 cluster solution
# get cluster means 
aggregate(df,by=list(fit$cluster),FUN=mean)
# append cluster assignment
dfc <- data.frame(df, fit$cluster)

# Ward Hierarchical Clustering
ddf <- dist(df, method = "euclidean") # distance matrix
fit <- hclust(ddf, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(df, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

# Model Based Clustering
library(mclust)
fit <- Mclust(df)
plot(fit) # plot results 
summary(fit) # display the best model

# K-Means Clustering with 5 clusters
fit <- kmeans(dfs, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(dfs, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(dfs, fit$cluster)

# comparing 2 cluster solutions
library(fpc)
cluster.stats(dfs, fit1$cluster, fit2$cluster)
