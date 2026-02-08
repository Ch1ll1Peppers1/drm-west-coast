#########################################################
# Shortbelly Rockfish DRM Catch Data
#
# Author: Luis Mario Ramirez Cruz
#
# Last time modified: 1/28/2026
#
# Any current bugs (Y/N)? N
#
# Description:
#   This worflow will help you clean and obtain the
#   rockfish data that you will require for the model
#
# Currently working in:
#
#
###################################################################
#HOW TO OBTAIN ALL DATA AND CREATE THE SUMMARY_CATCH FILE:
#-REQ: OISST_vignette.csv (Should be in data, if not look at OISST_DATA.R)
#-CATCH: OBTAINED FROM NWSF SURVEY DATA (Should be in data, if not refer to Workflow.R)

library(dplyr)

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
    y = sum(total_catch_numbers, na.rm = TRUE),
    lat = mean(Latitude_dd, na.rm = TRUE),
    lon = mean(Longitude_dd, na.rm = TRUE),
    depth = mean(Depth_m, na.rm = TRUE),
    n_hauls = length(unique(Trawl_id)),
    .groups = "drop"
  )
patch_areas <- data.frame(
  patch = c(1, 2, 3, 4),
  area_km2 = c(61238.287611, 18133.043869, 10286.465331, 27234.993392)  # VALUES CALCULATED AT Workflow.r
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
