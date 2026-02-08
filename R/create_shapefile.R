#Making a shapefile

library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(concaveman)

pts <- vect(catch, geom = c("Longitude_dd", "Latitude_dd"), crs = "EPSG:4326")
pts_sf <- st_as_sf(pts)
hull_sf <- concaveman(pts_sf, concavity = 6)

# CALI COASTLINE
coast <- ne_states(country = "united states of america", returnclass = "sf") %>%
  dplyr::filter(name == "California") %>%
  st_transform(4326)

# KEEP ONLY OCEAN PART
ocean_shape <- st_difference(hull_sf, st_union(coast))

bbox <- st_bbox(ocean_shape)

# Create 4 divisions from south to north
lat_breaks <- c(32, 34.52, 36.75, 38, 42)     #(32, 34.52, 36.75, 38, 42)

# Create polygons for each part
lat_polys <- lapply(1:4, function(i) {
  st_polygon(list(matrix(
    c(bbox["xmin"], lat_breaks[i],
      bbox["xmax"], lat_breaks[i],
      bbox["xmax"], lat_breaks[i+1],
      bbox["xmin"], lat_breaks[i+1],
      bbox["xmin"], lat_breaks[i]), 
    ncol = 2, byrow = TRUE)))
}) %>%
  st_sfc(crs = 4326) %>%
  st_sf(patch_id = 1:4)

# Getting 4 patches
ocean_patches <- st_intersection(ocean_shape, lat_polys)

st_write(ocean_patches, "Cali_4Patches.shp", delete_layer = TRUE)

