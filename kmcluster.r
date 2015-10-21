#install.packages('rgdal')
#install.packages('ggfortify')
#install.packages('fossil')
#install.packages('geosphere')
#library(WeightedCluster)

library(mclust)
library(cluster) 
library(fossil)
library(geosphere)
library(ggfortify)
library(rgdal)

#K-Mediods Clustering
clustering <- dataset
clustering$emw <- clustering$mwsig <- clustering$nstar <- clustering$comment <- 
  clustering$src <- clustering$declustered <- clustering$datetime <- clustering$mag <- 
  clustering$magtype <- clustering$nbstations <- clustering$gap <- clustering$distance <-
  clustering$rms <- clustering$source <- clustering$eventid <- clustering$network <-
  clustering$f_NE <- clustering$f_1997GSC <- clustering$f_1982NE <- NULL

Lon = clustering[1]
Lat = clustering[2]
#dcos = distCosine(c(0,0), cbind(Lon, Lat))
dhav = distHaversine(c(0,0), cbind(Lon, Lat))
#dvsp = distVincentySphere(c(0,0), cbind(Lon, Lat))
#par(mfrow=(c(1,2))) #create matrix
#dvse = distVincentyEllipsoid(c(0,0), cbind(Lon, Lat))
#plot(dvsp/1000, (dvsp-dvse)/1000, col='blue', xlab='Vincenty Sphere Distance (km)',
#     ylab="Difference between ")

#a is radius of Earth, f is earth's ellipsoid 
#clust <- data.matrix(clustering[1:2])
#p <- areaPolygon(clust, a=6378137, f=1/298.257223563)
fit <- pam(dhav,4)
clustering <- merge(clustering,fit$data)

#account for 3rd column
#d = geo.dist(data.frame(clustering), dist = TRUE)
#d <- geo.dist(clustering[1:2])
#hc <- hclust(d)
#clustering$clust <- cutree(hc,k=4)

#fit <- pam(clustering,2)
#deg.dist(43.5,-43,35.5,43.5)
#fit <- relational.clustering(clustering[1:2], clusters = 2)
#pam(daisy(x, metric = "manhattan"), 2, diss = TRUE)
pp <- ggplot() +
  geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
  geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
  annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
  geom_point(data=clustering, size=3, alpha = .7, aes(x=lon, y=lat, color=V1)) +
  scale_color_gradient(low="blue", high="red") +
  theme(plot.background = element_rect(fill = 'grey')) +
  geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)


#clusplot(clustering, fit$cluster, color=TRUE, shade=TRUE, 
#         labels=2, lines=0) +
#theme(plot.background = element_rect(fill = 'grey')) 
#geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)  
#scale_colour_manual(values=c("purple", "green","orange","black"))

pp
# print(pp)