library(WeightedCluster)

##K-Mediods Clustering
#Can use distance matrix above

coords2  <- data.frame(long=dataset$lon, lat=dataset$lat)
dist2 <- dist(coords2, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#dist2  <- earth.dist(df, dist=T)

#data(mvad)
## Aggregating state sequence
#aggMvad <- wcAggregateCases(mvad[, 17:86], weights=mvad$weight)
## Creating state sequence object
#mvad.seq <- seqdef(mvad[aggMvad$aggIndex, 17:86], weights=aggMvad$aggWeights)
## Computing Hamming distance between sequence
#diss <- seqdist(mvad.seq, method="HAM")

## K-Medoids
kmclust <- wcKMedoids(dist2, 25, weights=NULL, npass = 1, initialclust=NULL, method="KMedoids", cluster.only = FALSE, debuglevel=0)
#clust5 <- wcKMedoids(diss, k=5, weights=aggMvad$aggWeights)

## clust5$clustering contains index number of each medoids
## Those medoids are
unique(kmclust$clustering)

## Print the medoids sequences
#print(mvad.seq[unique(clust5$clustering), ], informat="SPS")
## Some info about the clustering
print(kmclust)
## Plot sequences according to clustering solution.
#seqdplot(mvad.seq, group=clust5$clustering)