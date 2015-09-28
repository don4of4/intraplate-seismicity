install.packages("maps")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("fpc", dependencies = TRUE)
install.packages("fossil")

library(ggplot2)
library(maps)
library(dplyr)
library(plyr)
library(fpc)
library(fossil)

# Utility function:  DO NOT EDIT
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}



data.neic <- read.table("data/2014_NEIC_declustered.c4.csv", header = TRUE, sep = ",")

data.anss <- read.table("data/anss.csv", header = TRUE, sep = ",")
colnames(data.anss) <- c("datetime","lat","lon","dp.km","mag","magtype","nbstations", "gap", "distance", "rms", "source", "eventid")

stations.iris <- read.table("data/out_fetchmdata_sept15", header = FALSE, sep = "|")
colnames(stations.iris) <- c("net","sta","loc","chan","lat","lon","elev","depth","azimuth","dip","instrument","scale","scalefreq","scaleunits","samplerate","start","end")


# Magnitude to mw
data.anss$emw <- ifelse(data.anss$magtype == "ML", 0.806*data.anss$mag + 0.633,
                   ifelse(data.anss$magtype == "Mb", 0.7813*data.anss$mag + 1.5175,
                   ifelse(data.anss$magtype == "Md", 0.806*data.anss$mag + 0.633,
                   ifelse(data.anss$magtype == "Mx", -1,
                   ifelse(data.anss$magtype == "Mh", -1,
                   ifelse(data.anss$magtype == "Mc",0.806*data.anss$mag + 0.633,
                   ifelse(data.anss$magtype == "Unk", -1, -2)))))))


#FIXME: Why do I have bad values?
data.anss <- subset(data.anss, data.anss$emw > 0)

# Date formatting
data.neic$datetime <- ISOdatetime(data.neic$y, data.neic$m, data.neic$d, data.neic$h, data.neic$m.1, data.neic$s, tz = "")
data.anss$datetime <- as.POSIXct(data.anss$datetime, tz = "")
stations.iris$start <- strptime(stations.iris$start,format="%Y-%m-%dT%H:%M:%OS")
stations.iris$end <- strptime(stations.iris$end,format="%Y-%m-%dT%H:%M:%OS")
#add miliseconds to format
#stations.iris$start <- options(digits.secs=3)
#stations.iris$end  <- options(digits.secs=3)
#options(stations.iris$start) #reset options
#options(stations.iris$end) #reset options

# Rename lon and lat
data.neic$lon <- data.neic$lon.dE.
data.neic$lat <- data.neic$lat.dN.
data.neic$lon.dE. <- data.neic$lat.dN. <- data.neic$y <- data.neic$m <- data.neic$d <- data.neic$h <- data.neic$m.1 <- data.neic$s <- NULL

# Filter based on lat and long
m <- rbind.fill(data.neic, data.anss)
m <- dplyr::bind_rows(data.neic, data.anss)

dataset <- subset(m, lat >= 35.5 & lat <= 43.5 & lon <= -71 & lon >= -84 )

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
  geom_abline(intercept = 3, slope = -.45, color = "blue", size = 1)

d <- dbscan(dataset, 10,showplot = 2)