
# using K-means

iris1<-iris[,1:4]
results<-kmeans(iris1,3)
results
table(iris$Species,results$cluster)
cat("The no. of elements in each cluster are",table(results$cluster))

plot(iris[,-5],col=results$cluster)

results4<-kmeans(iris1,4)
results4
table(iris$Species,results4$cluster)
cat("The no. of elements in each cluster are",table(results4$cluster))
plot(iris[,-5],col=results4$cluster)

# Using DbScan
install.packages("dbscan")
library(dbscan)
iris_1<-iris[,1:4]
kNNdistplot(iris_1,k=3)
abline(h=0.4,col="red")
db<-dbscan(iris_1,0.5,4)
db
hullplot(iris_1,db$cluster)
table(iris$Species,db$cluster)


# using Hierachical clustering


x<-hclust(dist(iris),method="complete")
plot(x,main=" Using Hierarchical Clustering ",cex=0.9)
y<-cutree(x,3)
y
table(y,iris$Species)
