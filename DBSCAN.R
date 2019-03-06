library(dbscan)
data(iris)
iris <- as.matrix(iris[,1:4])

## find suitable eps parameter using a k-NN plot for k = dim + 1
## Look for the knee!
kNNdistplot(iris, k = 5)
abline(h=.6, col = "red", lty=2)

# minpts is usually dim + 1
res <- dbscan(iris, eps = .6, minPts = 5)
res

# The method proposed here consists of computing the k-nearest neighbor distances in a matrix of points.
# The idea is to calculate, the average of the distances of every point to its k nearest neighbors. The value of k will be specified by the user and corresponds to MinPts.
# Next, these k-distances are plotted in an ascending order. The aim is to determine the “knee”, which corresponds to the optimal eps parameter.
# A knee corresponds to a threshold where a sharp change occurs along the k-distance curve.
# The function kNNdistplot() [in dbscan package] can be used to draw the k-distance plot