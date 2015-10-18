library(WeightedCluster)

#K-Mediods Clustering
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

pp <- ggplot() +
  geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
  geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
  annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
  geom_point(data=dataset, size=3, alpha = .7, aes(x=lon, y=lat, color=factor(kmclust))) +
  theme(plot.background = element_rect(fill = 'grey')) +
  geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1) + 
  scale_colour_manual(values=c("purple", "green","orange","black"))

 print(pp)

## Print the medoids sequences
#print(mvad.seq[unique(clust5$clustering), ], informat="SPS")
## Some info about the clustering
#print(kmclust)
## Plot sequences according to clustering solution.
#seqdplot(mvad.seq, group=clust5$clustering)