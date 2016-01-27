if (!require("pacman")) install.packages("pacman")
pacman::p_load("maps","dplyr","ggplot2","fpc","fossil","WeightedCluster","shinyRGL","scatterplot3d","ks","shiny")

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


# ISC small magnitude data
data.small_mag <- read.table("data/small_mag.txt", header = TRUE, sep = ",")
colnames(data.small_mag) <- c("datetime","lat","lon","depth","mag","magtype","nbstations", "gap", "distance", "rms", "source", "eventid")
data.small_mag$src <- 'Small_Mag'
data.small_mag$declustered <- FALSE

# ANF data import
data.anf <- read.table("data/ANF_06_15.txt", header = FALSE, sep = "")
colnames(data.anf) <- c('date','time','lon','lat','depth','emw','eventid')
data.anf$src <- 'ANF'
data.anf$declustered <- FALSE

# ISC arrivals data
data.arrivals <- read.table("data/isc_all_arrivals_no_restriction.txt", header = TRUE, sep = ",")
data.arrivals$datetime <- strptime(paste0(data.arrivals$DATE, "T", data.arrivals$TIME), format="%Y-%m-%dT%H:%M:%OS")

# Date formatting
options(digits.secs=3)
data.neic$datetime <- ISOdatetime(data.neic$y, data.neic$m, data.neic$d, data.neic$h, data.neic$m.1, data.neic$s, tz = "")
data.anss$datetime <- as.POSIXct(data.anss$datetime, tz = "")
data.small_mag$datetime <- as.POSIXct(strptime(data.small_mag$datetime, format="%Y/%m/%d %H:%M:%OS"))
data.anf$datetime <- as.POSIXct(strptime(paste0(data.anf$date, " ",data.anf$time), format="%m/%d/%Y %H:%M:%OS"))
stations.iris$start <- strptime(stations.iris$start,format="%Y-%m-%dT%H:%M:%OS")
stations.iris$end <- strptime(stations.iris$end,format="%Y-%m-%dT%H:%M:%OS")

# Strip leftover date/time metadata
data.anf$date <- data.anf$time <- NULL

# Rename lon and lat
data.neic$y <- data.neic$m <- data.neic$d <- data.neic$h <- data.neic$m.1 <- data.neic$s <- NULL

# NA Depth is intepreted as 0
data.anss$depth[is.na(data.anss$depth)] <- 0
data.small_mag$depth[is.na(data.small_mag$depth)] <- 0
data.anf$depth[is.na(data.anf$depth)] <- 0


## Magnitude to Mw

# Check event zone: This function is reusable, simply ensure needed variables are present. (datetime,source,lat,lon)
  check_event_zone <- function(data_in){
    data_in$f_NE <- ifelse(data_in$lat > -0.45*data_in$lon + 3, 1, 0)
    data_in$f_1997GSC <- ifelse(data_in$lat > -0.45*data_in$lon + 3, 1, 0)
    data_in$f_1982NE <- ifelse(data_in$f_NE == 1 && data_in$source != 'GSC' && format(data_in$datetime, "%Y") < 1982, 1, 0)
    return(data_in)
  }

data.anss <- check_event_zone(data.anss)
data.small_mag <- check_event_zone(data.small_mag)


# Magnitude conversion
  convert_mag <- function(data){
    data$emw <- ifelse(data$magtype == "ML", 0.806*data$mag + 0.633,
                     ifelse(data$magtype == "Mb", data$mag - 0.316 - 0.118*data$f_NE - 0.192*data$f_1997GSC + 0.280*data$f_1982NE,
                     ifelse(data$magtype == "Md", 0.806*data$mag + 0.633,
                     ifelse(data$magtype == "Mx", -1,
                     ifelse(data$magtype == "Mh", -1,
                     ifelse(data$magtype == "Mc", 0.806*data$mag + 0.633,
                     ifelse(data$magtype == "Unk", -1, -2)))))))
    data$mag <- data$magtype <- NULL # Cleanup source
    return(data)
  }

data.anss <- convert_mag(data.anss)
data.small_mag <- convert_mag(data.small_mag)

# Strip the helper information from check_event_zone
  cleanup_emw_conversion <- function(data){
    data$f_NE <- data$f_1997GSC <- data$f_1982NE <- NULL
    return(data)
  }

data.anss <- cleanup_emw_conversion(data.anss)
data.small_mag <- cleanup_emw_conversion(data.small_mag)

# Filter out tuples with unknown mag-
data.anss <- subset(data.anss, data.anss$emw > 0)
data.small_mag <- subset(data.small_mag, data.small_mag$emw > 0)
data.anf <- subset(data.anf, data.anf$emw > 0)

# Filter out based on lat and long  
# ANF EXCLUDED.  To insert put data.anf below
dataset <- dplyr::bind_rows(data.neic, data.anss, data.small_mag) # May coerce factors to char, this is okay.


# States within the region
target_states <- c( "pennsylvania", "new york", "new jersey", "virginia", "kentucky","rhode island",
                    "massachusetts","vermont","new hampshire", "delaware", "maryland", "west virginia", 
                    "north carolina", "tennessee", "ohio", "connecticut", "district of columbia" )
all_states <- map_data("state")
county <- map_data("county")
states <- subset(all_states, region %in% target_states)
county <- subset(county, region %in% target_states)


runApp(".") #automatically
