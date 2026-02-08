# Install if needed
# install.packages(c("ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata"))
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# DIVIDE INTO EQUAL CATCH NUMBER REGIONS
n_groups <- 6
catch_sorted <- catch[order(catch$Latitude_dd), ]
catch_sorted$cum_weight <- cumsum(catch_sorted$total_catch_numbers)
total_weight <- sum(catch_sorted$total_catch_numbers)
breaks <- seq(0, total_weight, length.out = n_groups + 1)

catch_sorted$region <- cut(
  catch_sorted$cum_weight,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("Region_", seq_len(n_groups))
)

# SUMMARIZE REGION (latitude)
region_summary <- catch_sorted %>%
  group_by(region) %>%
  summarise(
    min_lat = min(Latitude_dd),
    max_lat = max(Latitude_dd),
    min_lon = min(Longitude_dd),
    max_lon = max(Longitude_dd),
    total_catch_numbers = sum(total_catch_numbers)
  ) %>%
  mutate(
    legend_label = paste0(
      region, "\n",
      sprintf("Lat: %.2f°–%.2f°", min_lat, max_lat)
    )
  )

# MERGE SUMMARY INFO
catch_plot <- catch_sorted %>%
  left_join(region_summary, by = "region")

# COASTLINE
coast <- ne_countries(scale = "medium", returnclass = "sf")

# MAP LIMITS
xlim <- c(-126, -115.5)
ylim <- c(30, 43)

# BOUNDARIES
boundary_lines <- region_summary$max_lat[-nrow(region_summary)]

# PLOT
ggplot() +
  geom_sf(data = coast, fill = "gray90", color = "gray40", linewidth = 0.3) +
  geom_point(
    data = catch_plot,
    aes(x = Longitude_dd, y = Latitude_dd,
        color = legend_label),
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = boundary_lines,
    color = "red", linetype = "dashed", size = 0.7
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_color_discrete(name = "Region Information") +
  theme_minimal() +
  labs(
    title = "Equal-Catch Latitude Regions for Shortbelly Rockfish",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold")
  )

# ---- PRINT REGION SUMMARY TO CONSOLE ----
cat("\nRegion boundaries summary:\n")
region_summary_print <- region_summary %>%
  select(region, min_lat, max_lat, min_lon, max_lon) %>%
  mutate(
    min_lat = sprintf("%.5f", min_lat),
    max_lat = sprintf("%.5f", max_lat),
    min_lon = sprintf("%.5f", min_lon),
    max_lon = sprintf("%.5f", max_lon)
  )

print(region_summary_print, row.names = FALSE)
