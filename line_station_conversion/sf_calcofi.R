library(sf)

# lon/lat to line/station
# whip up some data
lldat <- data.frame(lon=c(-121.15, -121.2367),
                    lat=c(34.15, 34.5267))
lldat

# convert to an sf object and assign a crs
sfdat <- st_as_sf(lldat, coords=c('lon', 'lat'))
st_crs(sfdat) <- '+proj=longlat +ellps=clrk66'
# or
sfdat <- st_as_sf(lldat, coords=c('lon', 'lat'),
                  crs='+proj=longlat +ellps=clrk66')

# transform the geometry to calcofi coords and turn back into another dataframe
lsdat <- data.frame(st_coordinates(st_transform(
  sfdat, '+proj=calcofi +ellps=clrk66')))
names(lsdat) <- c('line', 'station')
lsdat

# back to lon/lat
data.frame(st_coordinates(st_transform(
  sfdat, '+proj=longlat +ellps=clrk66')))
