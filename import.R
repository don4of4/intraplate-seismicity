install.packages("maps")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("fpc", dependencies = TRUE)
install.packages("fossil")
install.packages("WeightedCluster")

library(ggplot2)
library(maps)
library(plyr)
library(dplyr)
library(fpc)
library(fossil)
library(WeightedCluster)

data.neic <- read.table("data/NEIC_HM_2014.csv", header = TRUE, sep = ",")
data.neic$src <- 'HM_2014'
data.neic$declustered <- TRUE

data.anss <- read.table("data/ANSS_2013.csv", header = TRUE, sep = ",")
colnames(data.anss) <- c("datetime","lat","lon","dp.km","mag","magtype","nbstations", "gap", "distance", "rms", "source", "eventid")
data.anss$src <- 'ANSS_2013'
data.anss$declustered <- FALSE

stations.iris <- read.table("data/out_fetchmdata_sept15", header = FALSE, sep = "|")
colnames(stations.iris) <- c("net","sta","loc","chan","lat","lon","elev","depth","azimuth","dip","instrument","scale","scalefreq","scaleunits","samplerate","start","end")
data.anss$network <- 'IRIS'

data.small_mag <- read.table("data/small_mag_ISC_75_2012.txt", header = TRUE, sep = "\t")
colnames(data.small_mag) <- c("eventid", "author", "date","time", "lat","lon", "depth","depfix","magauthor","magtype","mag", "magauthor2", "magtype2", "mag2", "magauthor3", "magtype3", "mag3")
data.small_mag$src <- 'Small_Mag'
data.small_mag$declustered <- FALSE


# Date formatting
  options(digits.secs=3)
  data.neic$datetime <- ISOdatetime(data.neic$y, data.neic$m, data.neic$d, data.neic$h, data.neic$m.1, data.neic$s, tz = "")
  data.anss$datetime <- as.POSIXct(data.anss$datetime, tz = "")
  stations.iris$start <- strptime(stations.iris$start,format="%Y-%m-%dT%H:%M:%OS")
  stations.iris$end <- strptime(stations.iris$end,format="%Y-%m-%dT%H:%M:%OS")
  data.small_mag$datetime <- paste0(data.small_mag$date, "T", data.small_mag$time)
  #strptime(data.small_mag$datetime, format="%m/%d/%yT%H:%M:%OS")
  # FIXME: Hours in this data is invalid; most tuples are > 24.  Why?

# Rename lon and lat
  data.neic$lon <- data.neic$lon.dE.
  data.neic$lat <- data.neic$lat.dN.
  data.neic$lon.dE. <- data.neic$lat.dN. <- data.neic$y <- data.neic$m <- data.neic$d <- data.neic$h <- data.neic$m.1 <- data.neic$s <- NULL
  

# Magnitude to mw
  # Check event zone.
  data.anss$f_NE <- ifelse(data.anss$lat > -0.45*data.anss$lon + 3, 1, 0)
  data.anss$f_1997GSC <- ifelse(data.anss$lat > -0.45*data.anss$lon + 3, 1, 0)
  data.anss$f_1982NE <- ifelse(data.anss$NE == 1 && data.anss$source != 'GSC' && format(data.anss$datetime, "%Y") < 1982, 1, 0)

  data.anss$emw <- ifelse(data.anss$magtype == "ML", 0.806*data.anss$mag + 0.633,
                   ifelse(data.anss$magtype == "Mb", data.anss$mag - 0.316 - 0.118*data.anss$f_NE - 0.192*data.anss$f_1997GSC + 0.280*data.anss$f_1982NE,
                   ifelse(data.anss$magtype == "Md", 0.806*data.anss$mag + 0.633,
                   ifelse(data.anss$magtype == "Mx", -1,
                   ifelse(data.anss$magtype == "Mh", -1,
                   ifelse(data.anss$magtype == "Mc",0.806*data.anss$mag + 0.633,
                   ifelse(data.anss$magtype == "Unk", -1, -2)))))))

  # Filter out tuples with unknown mag
  data.anss <- subset(data.anss, data.anss$emw > 0)

# Filter out based on lat and long
  m <- rbind.fill(data.neic, data.anss)
  m <- dplyr::bind_rows(data.neic, data.anss)
  dataset <- subset(m, lat >= 35.5 & lat <= 43.5 & lon <= -71 & lon >= -84)

# Calculate distance matrix for the purposes of clustering.
coordinates  <- data.frame(long=dataset$lon, lat=dataset$lat)
dist  <- earth.dist(df, dist=T)
dens<-dbscan(dist,MinPts=25,eps=0.43,method="dist")


target_states <- c( "pennsylvania", "new york", "new jersey", "virginia", "kentucky","rhode island",
                    "massachusetts","vermont","new hampshire", "delaware", "maryland", "west virginia", 
                    "north carolina", "tennessee", "ohio", "connecticut", "district of columbia" )
all_states <- map_data("state")
county <- map_data("county")

states <- subset(all_states, region %in% target_states)
county <- subset(county, region %in% target_states)

p <- ggplot() +
  geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
  geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
  annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
  geom_point(size=2, alpha = .7, aes(dataset$lon, dataset$lat, color=dataset$emw)) +
  scale_color_gradient(low="blue", high="red") +
  theme(plot.background = element_rect(fill = 'grey')) +
  geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)

d <- dbscan(dataset, 10,showplot = 2)


##K-Mediods Clustering
#Can use distance matrix above

coordinates2  <- data.frame(long=dataset$lon, lat=dataset$lat)
dist2  <- earth.dist(df, dist=T)

#data(mvad)
## Aggregating state sequence
#aggMvad <- wcAggregateCases(mvad[, 17:86], weights=mvad$weight)
## Creating state sequence object
#mvad.seq <- seqdef(mvad[aggMvad$aggIndex, 17:86], weights=aggMvad$aggWeights)
## Computing Hamming distance between sequence
#diss <- seqdist(mvad.seq, method="HAM")

## K-Medoids
kmclust <- wcKMedoids(dist2, 25, weights=NULL, npass = 1, initialclust=25, method="KMedoids", cluster.only = FALSE, debuglevel=0)
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

