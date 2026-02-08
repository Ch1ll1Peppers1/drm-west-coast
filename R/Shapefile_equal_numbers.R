library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(units)

# --- Use your existing region_summary ---
# region_summary should have: region, min_lat, max_lat, min_lon, max_lon

# --- Get California coastline polygon ---
coast <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(name == "California") %>%
  st_transform(crs = 4326)

# --- Create rectangular polygons for each region ---
region_polys <- lapply(1:nrow(region_summary), function(i) {
  r <- region_summary[i, ]
  coords <- matrix(
    c(r$min_lon, r$min_lat,
      r$min_lon, r$max_lat,
      r$max_lon, r$max_lat,
      r$max_lon, r$min_lat,
      r$min_lon, r$min_lat),
    ncol = 2,
    byrow = TRUE
  )
  st_polygon(list(coords))
})

regions_sf <- st_sf(
  region = region_summary$region,
  min_lat = region_summary$min_lat,
  max_lat = region_summary$max_lat,
  min_lon = region_summary$min_lon,
  max_lon = region_summary$max_lon,
  total_catch_numbers = region_summary$total_catch_numbers,
  geometry = st_sfc(region_polys),
  crs = 4326
)

# --- Subtract the California landmass to keep only ocean region ---
regions_ocean <- st_difference(st_union(regions_sf), st_union(coast))

# Convert back to individual regions (clip each polygon)
regions_ocean_split <- st_intersection(regions_sf, regions_ocean)

# --- Reproject for area calculation ---
regions_ocean_area <- st_transform(regions_ocean_split, 3310)  # California Albers equal area

# --- Compute area in km² ---
regions_ocean_area$area_km2 <- set_units(st_area(regions_ocean_area), "km^2")
regions_ocean_area$area_km2 <- as.numeric(regions_ocean_area$area_km2)

# --- Print summary ---
cat("\nRegion offshore areas (km²):\n")
print(regions_ocean_area %>% 
        st_drop_geometry() %>% 
        select(region, area_km2, total_catch_numbers))

# --- Export shapefile (clean fields only) ---
regions_export <- regions_ocean_area %>%
  select(region, min_lat, max_lat, min_lon, max_lon, total_catch_numbers, area_km2)

st_write(regions_export, "equal_catch_regions_ocean_california.shp", delete_layer = TRUE)

cat("\nShapefile createdn")
