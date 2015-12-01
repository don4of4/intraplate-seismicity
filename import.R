if (!require("pacman")) install.packages("pacman")
pacman::p_load("maps","dplyr","ggplot2","fpc","fossil","WeightedCluster","shinyRGL","scatterplot3d","ks")

# NEIC data set
data.neic <- read.table("data/NEIC_HM_2014.csv", header = TRUE, sep = ",")
colnames(data.neic) <- c("emw","lon","lat","depth","y","m","d", "h", "m.1", "s", "mwsig", "nstar","comment")
data.neic$src <- 'HM_2014'
data.neic$declustered <- TRUE

# ANSS data set
data.anss <- read.table("data/ANSS_2013.csv", header = TRUE, sep = ",")
colnames(data.anss) <- c("datetime","lat","lon","depth","mag","magtype","nbstations", "gap", "distance", "rms", "source", "eventid")
data.anss$src <- 'ANSS_2013'
data.anss$declustered <- FALSE

# IRIS stations data
stations.iris <- read.table("data/all_stn_metadata_oct15", header = FALSE, sep = "|")
colnames(stations.iris) <- c("net","sta","loc","chan","lat","lon","elev","depth","azimuth","dip","instrument","scale","scalefreq","scaleunits","samplerate","start","end")
data.anss$network <- 'IRIS'

# ISC magnitude data
data.small_mag <- read.table("data/small_mag_ISC_75_2012.txt", header = TRUE, sep = "\t")
colnames(data.small_mag) <- c("eventid", "author", "date","time", "lat","lon", "depth","depfix","magauthor","magtype","mag", "magauthor2", "magtype2", "mag2", "magauthor3", "magtype3", "mag3")
data.small_mag$src <- 'Small_Mag'
data.small_mag$declustered <- FALSE

# ISC arrivals data
data.arrivals <- read.table("data/isc_all_arrivals_no_restriction.txt", header = TRUE, sep = ",")
data.arrivals$datetime <- strptime(paste0(data.arrivals$DATE, "T", data.arrivals$TIME), format="%Y-%m-%dT%H:%M:%OS")

# Date formatting
options(digits.secs=3)
data.neic$datetime <- ISOdatetime(data.neic$y, data.neic$m, data.neic$d, data.neic$h, data.neic$m.1, data.neic$s, tz = "")
data.anss$datetime <- as.POSIXct(data.anss$datetime, tz = "")
stations.iris$start <- strptime(stations.iris$start,format="%Y-%m-%dT%H:%M:%OS")
stations.iris$end <- strptime(stations.iris$end,format="%Y-%m-%dT%H:%M:%OS")
data.small_mag$datetime <- paste0(data.small_mag$date, "T", data.small_mag$time)

# Rename lon and lat
data.neic$y <- data.neic$m <- data.neic$d <- data.neic$h <- data.neic$m.1 <- data.neic$s <- NULL


## Magnitude to Mw

# Check event zone.
data.anss$f_NE <- ifelse(data.anss$lat > -0.45*data.anss$lon + 3, 1, 0)
data.anss$f_1997GSC <- ifelse(data.anss$lat > -0.45*data.anss$lon + 3, 1, 0)
data.anss$f_1982NE <- ifelse(data.anss$NE == 1 && data.anss$source != 'GSC' && format(data.anss$datetime, "%Y") < 1982, 1, 0)

# Magnitude conversion
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

# States within the region
target_states <- c( "pennsylvania", "new york", "new jersey", "virginia", "kentucky","rhode island",
                    "massachusetts","vermont","new hampshire", "delaware", "maryland", "west virginia", 
                    "north carolina", "tennessee", "ohio", "connecticut", "district of columbia" )
all_states <- map_data("state")
county <- map_data("county")
states <- subset(all_states, region %in% target_states)
county <- subset(county, region %in% target_states)


runApp(".") #automatically