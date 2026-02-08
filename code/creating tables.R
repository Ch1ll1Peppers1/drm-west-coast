


nwsfcdat |>
  ggplot(aes(total_catch_numbers)) +
  geom_histogram()

nwsfcdat |>
  dplyr::filter(total_catch_numbers > 0) |>
  ggplot(aes(total_catch_numbers)) +
  geom_histogram()


ggplot(nwsfcdat, aes(Longitude_dd, Latitude_dd)) +
  geom_point(aes(size = total_catch_numbers, color = total_catch_numbers)) +
  scale_color_viridis_c() +
  coord_map()

remotes::install_github("sjevelazco/adm")         # Install adm package from GitHub
remotes::install_github("sjevelazco/flexsdm")     # Install flexsdm package from GitHub

require(adm) # Load adm package for abundance-based SDMs
require(flexsdm) # Load flexsdm for flexible SDMs
require(terra)
require(tidyterra) # Load tidyterra for tidy interaction with terra objects


ca <- flexsdm::calib_area(
  data = nwsfcdat |> dplyr::filter(total_catch_numbers > 0), # Only species presence records
  x = "Longitude_dd", # Longitude column
  y = "Latitude_dd", # Latitude column
  method = c("buffer", width = 200000), # Create a buffer of 200 km radius
  crs = crs() # Use the CRS of the environmental data
)




