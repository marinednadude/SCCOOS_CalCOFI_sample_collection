
```{R}
install.packages("tidysdm")
library(tidysdm)
data(lacerta)
head(lacerta)

# download presences
library(rgbif)
occ_download_get(key = "0068808-230530130749713", path = tempdir())
# read file
library(readr)
distrib <- read_delim(file.path(tempdir(), "0068808-230530130749713.zip"))
# keep the necessary columns and rename them
lacerta <- distrib %>%
  select(gbifID, decimalLatitude, decimalLongitude) %>%
  rename(ID = gbifID, latitude = decimalLatitude, longitude = decimalLongitude)
# clean up the data
library(CoordinateCleaner)
lacerta <- clean_coordinates(
  x = lacerta,
  lon = "longitude",
  lat = "latitude",
  species = "ID",
  value = "clean"
)

library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
lacerta <- st_as_sf(lacerta, coords = c("longitude", "latitude"))
st_crs(lacerta) <- "+proj=longlat"

library(pastclim)
set_data_path(here())
pastclim::download_dataset(dataset = "WorldClim_2.1_10m")
land_mask <-
  get_land_mask(time_ce = 1985, dataset = "WorldClim_2.1_10m")

# Iberia peninsula extension
iberia_poly <-
  terra::vect(
    "POLYGON((-9.8 43.3,-7.8 44.1,-2.0 43.7,3.6 42.5,3.8 41.5,1.3 40.8,0.3 39.5,
     0.9 38.6,-0.4 37.5,-1.6 36.7,-2.3 36.3,-4.1 36.4,-4.5 36.4,-5.0 36.1,
    -5.6 36.0,-6.3 36.0,-7.1 36.9,-9.5 36.6,-9.4 38.0,-10.6 38.9,-9.5 40.8,
    -9.8 43.3))"
  )

crs(iberia_poly) <- "+proj=longlat"
# crop the extent
land_mask <- crop(land_mask, iberia_poly)
# and mask to the polygon
land_mask <- mask(land_mask, iberia_poly)
library(tidyterra)
#> 
#> Attaching package: 'tidyterra'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(ggplot2)
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta) +
  guides(fill = "none")
iberia_proj4 <-
  "+proj=aea +lon_0=-4.0 +lat_1=36.8 +lat_2=42.6 +lat_0=39.7 +datum=WGS84 +units=m +no_defs"

land_mask <- terra::project(land_mask, y = iberia_proj4)
lacerta <- st_transform(lacerta, iberia_proj4)
ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta) +
  guides(fill = "none")

set.seed(1234567)
lacerta <- thin_by_cell(lacerta, raster = land_mask)
nrow(lacerta)
#> [1] 233
#> 
pres_data <- terra::extract(land_mask, lacerta)
summary(pres_data)

ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta) +
  guides(fill = "none")

set.seed(1234567)
lacerta_thin <- thin_by_dist(lacerta, dist_min = km2m(20))
nrow(lacerta_thin)

ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta_thin) +
  guides(fill = "none")

library(rgbif)
occ_download_get(key = "0121761-240321170329656", path = tempdir())
library(readr)
backg_distrib <- readr::read_delim(file.path(
  tempdir(),
  "0121761-240321170329656.zip"
))
# keep the necessary columns
lacertidae_background <- backg_distrib %>%
  select(gbifID, decimalLatitude, decimalLongitude) %>%
  rename(ID = gbifID, latitude = decimalLatitude, longitude = decimalLongitude)

lacertidae_background <- st_as_sf(lacertidae_background,
                                  coords = c("longitude", "latitude")
)

st_crs(lacertidae_background) <- "+proj=longlat"
lacertidae_background <- st_transform(lacertidae_background, crs = iberia_proj4)


lacertidae_background_raster <- rasterize(lacertidae_background,
                                          land_mask,
                                          fun = "count"
)
lacertidae_background_raster <- mask(
  lacertidae_background_raster,
  land_mask
)
ggplot() +
  geom_spatraster(data = lacertidae_background_raster, aes(fill = count)) +
  scale_fill_viridis_b(na.value = "transparent")

guides(fill = "none")

set.seed(1234567)
lacerta_thin <- sample_background(
  data = lacerta_thin, raster = lacertidae_background_raster,
  n = 3 * nrow(lacerta_thin),
  method = "bias",
  class_label = "background",
  return_pres = TRUE
)

ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_1985)) +
  geom_sf(data = lacerta_thin, aes(col = class)) +
  guides(fill = "none")

```