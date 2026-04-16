`.deg2rad` <- function(deg) deg * pi / 180

`.rad2deg` <- function(rad) rad * 180 / pi

`.inverse.mercator` <- function(mercatorLat, iterations = 3)
{
    approxLat <- mercatorLat
    iterlatitude <- function(mercatorLat, approxLat)
    {
        approxLat <- 2 * (atan(exp(.deg2rad(mercatorLat) + 
            0.00676866 * sin(.deg2rad(approxLat)))) *  180 / pi - 45)
        approxLat
    }
    for (i in 1:iterations) approxLat <- iterlatitude(mercatorLat, 
         approxLat)
    approxLat
}

`.to.mercator` <- function(latitude)
{
    y <-  .rad2deg(log(tan(.deg2rad(45 + latitude / 2))) - 
                  0.00676866 * sin(.deg2rad(latitude)))
    y
}


`station.to.latlon` <- function(x, roundLines = TRUE)
{
    if (length(x) == 2 & class(x) != 'matrix'){
        x <- matrix(x, 1, 2)
    }
    line <- x[, 1]
    station <- x[, 2]
    
    refLatitude <- 34.15 -  0.2 * (line - 80) * cos(.deg2rad(30))
    latitude <- refLatitude - (station - 60) * sin(.deg2rad(30)) / 15
    L1 <- (.to.mercator(latitude) - .to.mercator(34.15)) * tan(.deg2rad(30))
    L2 <- (.to.mercator(refLatitude) - .to.mercator(latitude)) / 
          (cos(.deg2rad(30)) * sin(.deg2rad(30)))
    longitude <- -1 * (L1 + L2 + 121.15)
    cbind(lon = longitude, lat = latitude)
}

`latlon.to.station` <- function(x)
{
    if (length(x) == 2 & class(x) != 'matrix'){
        x <- matrix(x, 1, 2)
    }
    longitude <- x[, 1]
    latitude <- x[, 2]
    
    # assume we're in the western hemispere
    longitude[longitude > 180] <- -1 * (longitude[longitude > 180] - 360)
    longitude[longitude < 0] <- longitude[longitude < 0] * -1
     
    L1 <- (.to.mercator(latitude) - .to.mercator(34.15)) * tan(.deg2rad(30))
    L2 <- longitude - L1 - 121.15
    
    mercRefLatitude <- L2 * cos(.deg2rad(30)) * sin(.deg2rad(30)) + 
        .to.mercator(latitude)
    refLatitude <- .inverse.mercator(mercRefLatitude)
    line <- 80 - (refLatitude - 34.15) * 5 / cos(.deg2rad(30))
    station <- 60 + (refLatitude - latitude) * 15 / sin(.deg2rad(30))
    
    cbind(line = line, station = station)
}

