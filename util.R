if (!require("pacman")) install.packages("pacman")
pacman::p_load("proj4")

findDist <- function(x=c(0,0,0), y=c(0,0,0)) {
  x <- matrix(x/c(180/pi, 180/pi, 1), ncol=3)
  y <- matrix(y/c(180/pi, 180/pi, 1), ncol=3)
  xx <- ptransform(x, src.proj="+proj=longlat", dst.proj="+proj=geocent")
  yy <- ptransform(y, src.proj="+proj=longlat", dst.proj="+proj=geocent")
  sqrt(sum((yy-xx)^2))
}


# Copied from G-Library
as_radians = function(theta=0){
  return(theta * pi / 180)
}

# For calculations of distance
# TODO consider depth
calc_dist = function(from, to) {
  lat1 = as_radians(from$lat)
  lon1 = as_radians(from$lon)
  lat2 = as_radians(to$lat)
  lon2 = as_radians(to$lon)
  a = 3963.191;
  b = 3949.903;
  numerator = ( a^2 * cos(lat2) )^2 + ( b^2 * sin(lat2) ) ^2
  denominator = ( a * cos(lat2) )^2 + ( b * sin(lat2) )^2
  
  # Ellipticity of the earth
  radiusofearth = sqrt(numerator/denominator) 
  
  d = radiusofearth * acos( sin(lat1) * sin(lat2) + cos(lat1)*cos(lat2)*cos(lon2 - lon1) )
  d.return = list(distance_miles=d)
  return(d.return)
}

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