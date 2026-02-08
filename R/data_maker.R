library(dplyr)

# --- 1. Merge the two data frames using tow_id ---
merged <- catch %>%
  left_join(bio, by = "Trawl_id")

# --- 2. Define your patches by latitude ---
# Example: 4 patches between 32–48 degrees
patch_breaks <- c(32, 34.52, 36.75, 38, 42)
patch_labels <- 1:(length(patch_breaks) - 1)

merged <- merged %>%
  rename(
    Latitude_dd = Latitude_dd.x,
    Longitude_dd = Longitude_dd.x,
    Depth_m = Depth_m.x,
    Year = Year.x
  ) %>%
  select(-ends_with(".y"))

merged <- merged %>%
  mutate(
    patch = cut(Latitude_dd,
                breaks = patch_breaks,
                labels = patch_labels,
                include.lowest = TRUE,
                right = FALSE)
  )

summary_merged <- merged %>%
  group_by(patch, Year) %>%
  summarise(
    Age = mean(Age_years, na.rm = TRUE),
    y = sum(total_catch_numbers, na.rm = TRUE),
    lat = mean(Latitude_dd, na.rm = TRUE),
    lon = mean(Longitude_dd, na.rm = TRUE),
    depth = mean(Depth_m, na.rm = TRUE),
    n_hauls = length(unique(Trawl_id)),
    .groups = "drop"
  )
patch_areas <- data.frame(
  patch = c(1, 2, 3, 4),
  area_km2 = c(61238.287611, 18133.043869, 10286.465331, 27234.993392)  # example values
)

summary_merged <- summary_merged %>%
  mutate(patch = as.character(patch))

patch_areas <- patch_areas %>%
  mutate(patch = as.character(patch))

summary_merged <- summary_merged %>%
  left_join(patch_areas, by = "patch")


#-------------------
catch_base <- catch %>%
  mutate(
    patch = cut(Latitude_dd,
                breaks = patch_breaks,
                labels = patch_labels,
                include.lowest = TRUE,
                right = FALSE)
  )
summary_catch_base <- catch_base %>%
  group_by(patch, Year) %>%
  summarise(
#    Age = mean(Age_years, na.rm = TRUE),
    y = sum(total_catch_numbers, na.rm = TRUE),
    lat = mean(Latitude_dd, na.rm = TRUE),
    lon = mean(Longitude_dd, na.rm = TRUE),
    depth = mean(Depth_m, na.rm = TRUE),
    n_hauls = length(unique(Trawl_id)),
    .groups = "drop"
  )
patch_areas <- data.frame(
  patch = c(1, 2, 3, 4),
  area_km2 = c(61238.287611, 18133.043869, 10286.465331, 27234.993392)  # example values
)

summary_catch_base <- summary_catch_base %>%
  mutate(patch = as.character(patch))

patch_areas <- patch_areas %>%
  mutate(patch = as.character(patch))

summary_catch_base <- summary_catch_base %>%
  left_join(patch_areas, by = "patch")

summary_catch_base <- summary_catch_base %>%
  arrange(patch, Year)

summary_catch_base <- summary_catch_base %>%
  mutate(patch = as.numeric(patch))

summary_catch_base <- summary_catch_base %>%
  left_join(yearly_oisst_temp, by = c("patch", "Year"))


#-------------------


# --- 4. (Optional) Reorder or clean up columns ---
summary_df <- summary_df %>%
  arrange(patch, Year)
