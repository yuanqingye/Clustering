library(gclus)
data(wine)
head(wine)

dataset <- wine[,-1] #去除分类标签
dataset <- scale(dataset)

#1.Em method on gaussian model
library(mclust)
m_clust <- Mclust(as.matrix(dataset), G=1:20) #聚类数目从1一直试到20
summary(m_clust)

plot(m_clust, "BIC")
#best cluster 2 or 5

#2.try diff kmeans and using inner method to tell the best cluster number
library(NbClust)
set.seed(1234) #因为method选择的是kmeans，所以如果不设定种子，每次跑得结果可能不同
nb_clust <- NbClust(dataset,  distance = "euclidean",
                    min.nc=2, max.nc=15, method = "kmeans",
                    index = "alllong", alphaBeale = 0.1)
#best cluster 3

barplot(table(nb_clust$Best.nc[1,]),xlab = "聚类数",ylab = "支持指标数")


#3.using SSE to determine the correct cluster number 
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(dataset)

#existing method to do SSE comparison
library(factoextra)
library(ggplot2)
set.seed(1234)
fviz_nbclust(dataset, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

km.res <- kmeans(dataset,3)
fviz_cluster(km.res, data = dataset)


#4.K Medoids clustering
library(fpc)
pamk.best <- pamk(dataset)
pamk.best$nc

library(cluster)
clusplot(pam(dataset, pamk.best$nc))


#5.Calinsky criterion
library(vegan)
ca_clust <- cascadeKM(dataset, 1, 10, iter = 1000)
ca_clust$results

# calinski:
#   (SSB/(K-1))/(SSW/(n-K)), where n is the number of data points and K is the number of clusters. 
# SSW is the sum of squares within the clusters while SSB is the sum of squares among the clusters. 
# This index is simply an F (ANOVA) statistic.

#6.Affinity propagation (AP) clustering
library(apcluster)
ap_clust <- apcluster(negDistMat(r=2), dataset)
length(ap_clust@clusters)

#7.轮廓系数Average silhouette method
require(cluster)
library(factoextra)
fviz_nbclust(dataset, kmeans, method = "silhouette")

#8.Gap Statistic
library(cluster)
set.seed(123)
gap_clust <- clusGap(dataset, kmeans, 10, B = 500, verbose = interactive())
gap_clust

#9.层次聚类
h_dist <- dist(as.matrix(dataset))
h_clust<-hclust(h_dist)
plot(h_clust, hang = -1, labels = FALSE) #This plot is not working due to some reason
rect.hclust(h_clust,3)
