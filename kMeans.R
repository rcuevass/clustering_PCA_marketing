#Load data to data frame
data<-read.csv("data.csv")
# Take a look at the data...
View(data)
# Explore names of columns
names(data)
# Number of rows
nrow(data)

#Let's attempt to do clustering on the amounts purchased in the 
# different stores. For this goal we subset our data frame
vars<- c("customer_id","amount_purchased_shop_1","amount_purchased_shop_2",
         "amount_purchased_shop_3","amount_purchased_shop_4",
         "amount_purchased_shop_5")
df<- data[vars]
# Rename columns for simplicity
names(df)[2]<-"amount_1"
names(df)[3]<-"amount_2"
names(df)[4]<-"amount_3"
names(df)[5]<-"amount_4"
names(df)[6]<-"amount_5"
# check changes
names(df)

# Let's scale data for clustering. Notice we exclude the first column (-1)
# (customer_id) to do the scaling
df.scaled<-scale(df[c(-1)])
cor(df.scaled)
summary(df.scaled)

# The following lines are coded in order to SHOW the number of
# clusters suggested by the generated plot - we are looking for
# the elbow
# A plot of the within groups sum of squares by number of clusters
# extracted can help determine the appropriate number of clusters.
# This analysis looks for a bend in the plot. 
# We make sure the wss variable is free
rm(wss)

wss <- (nrow(df.scaled)-1)*sum(apply(df.scaled,2,var))

# Max number of clusters to explore
maxClusters <- 10
for (i in 1:maxClusters){
    # We determine the within sums for evey cluser analysis
    # executed (in this case, with i centers) and
    # store it in the ith component of wss
    wss[i] <- sum(kmeans(df.scaled,centers=i)$withinss)
}

# We plot the value of within-cluster sum of squares
# versus the number of clusters.
plot(1:maxClusters, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# We save the plot to file "elbow.png"
dev.copy(png, file="elbow.png", height=480, width=480)
## and switch off the device
dev.off()

# We plot, once again, the value of within-cluster sum of squares
# versus the number of clusters. This time we add the value
# of the sum of squares as a label to each point in the graph...
plot(1:maxClusters, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# We add values of points to make the plot clearer
# For doing so wee need the library "calibrate"
library(calibrate)
# We do the actual labeling
textxy(1:maxClusters, wss, signif(wss,digits=5), cex=0.8, xlab="Number of Clusters",
       ylab="Within groups sum of squares")
# We save the plot to file "elbow_labeled.png"
dev.copy(png, file="elbow_labeled.png", height=580, width=580)
## and switch off the device
dev.off()

# Let's choose the "ideal" number of clusters. Based on the previous
# plots we noticed that 5 would be a good number of clusters
nClusters <- 5
# We make sure the "ClusterResults" data frame is "free"
rm(ClusterResults)
# We perform the clustering itself
ClusterResults<-kmeans(df.scaled,nClusters)
# We remind ourselves of the content of it
names(ClusterResults)

# We generate a table that displays to what cluster each customer
# belongs to.
names(df)
cluster_table<-table(df$customer_id,ClusterResults$cluster)
head(cluster_table)
# We write this table to a csv file
write.csv(cluster_table,file="clustered_customers.csv")

# We generate a plot to visualize "clustering" in terms of 
# amount_1 and amount_3. This can be done for every pair of
# amount_x and amount_y and this one is generated only for
# exploratory purpouses.

plot(df[c("amount_1","amount_3")],col=ClusterResults$cluster,
     main="Clustering on amounts purchased on stores",
     sub="Amount spent in store 3 vs. amount spent in store 1",
     xlab="Amount spent in store 1",ylab="Amount spent in store 3")

# We save the plot to file "elbow.png"
dev.copy(png, file="clustering_1_vs_3.png", height=480, width=480)
## and switch off the device
dev.off()

# We now proceed to show the mean each cluster spent 
# We first aggregate our data frame according the clusters we just 
# found
#ClusterResults$cluster
aggrTable<-aggregate(df[c(-1)], by = list(ClusterResults$cluster),mean)

# We generate a bar plot to show amount spent per cluster ...
barplot(t(aggrTable[,-1]), main="Amount purchased by cluster", ylab="Total", 
        col=terrain.colors(5), space=0.1, cex.axis=0.8, las=1,
        names.arg=c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5"), cex=0.8)

# and save plot to file

# We save the plot to file "elbow.png"
dev.copy(png, file="bar_plot_mean_spent_per_cluster.png", height=480, width=480)
## and switch off the device
dev.off()


