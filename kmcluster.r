#library(WeightedCluster)
library(mclust)
library(cluster) 
install.packages('ggfortify')
require(ggfortify)
#K-Mediods Clustering
clustering <- dataset
clustering$emw <- clustering$mwsig <- clustering$nstar <- clustering$comment <- 
  clustering$src <- clustering$declustered <- clustering$datetime <- clustering$mag <- 
  clustering$magtype <- clustering$nbstations <- clustering$gap <- clustering$distance <-
  clustering$rms <- clustering$source <- clustering$eventid <- clustering$network <-
  clustering$f_NE <- clustering$f_1997GSC <- clustering$f_1982NE <- NULL
#clustering <- na.omit(clustering) # listwise deletion of missing
clustering <- scale(clustering) # standardize variables
fit <- Mclust(clustering)
fit <- kmeans(clustering, 5)


#coords2  <- data.frame(long=dataset$lon, lat=dataset$lat)
#dist2 <- dist(coords2, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

## K-Medoids
#kmclust <- wcKMedoids(dist2, 25, weights=NULL, npass = 1, initialclust=NULL, method="KMedoids", cluster.only = FALSE, debuglevel=0)

## clust5$clustering contains index number of each medoids
## Those medoids are
#unique(kmclust$clustering)
clusplot(clustering, fit$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0)  
pp <- clusplot(clustering, fit$cluster, color=TRUE, shade=TRUE, labels=0, lines=0) +
  geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
  geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
  annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01")
  #geom_point(data=dataset, size=3, alpha = .7, aes(x=lon, y=lat, color=emw))

  #clusplot(clustering, fit$cluster, color=TRUE, shade=TRUE, 
  #         labels=2, lines=0) +
#theme(plot.background = element_rect(fill = 'grey')) 
  #geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)  
  #scale_colour_manual(values=c("purple", "green","orange","black"))

pp
# print(pp)