#OBTAINING TEMPERATURE DATA
#https://cran.r-project.org/web/packages/heatwaveR/vignettes/OISST_preparation.html
# The packages we will need
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("tidync")
# install.packages("doParallel")
# install.packages("rerddap")
# install.packages("plyr")

library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing

# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# Note that there is also a version with lon values from 0 yo 360
rerddap::info(datasetid = "ncdcOisst21Agg", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

OISST_sub_dl <- function(time_df, retries = 5) {
  for (i in seq_len(retries)) {
    
    out <- try(
      rerddap::griddap(
        datasetx = "ncdcOisst21Agg_LonPM180",
        url = "https://coastwatch.pfeg.noaa.gov/erddap/",
        time = c(time_df$start, time_df$end),
        zlev = c(0, 0),
        latitude = c(42, 31),
        longitude = c(-125, -117),
        fields = "sst"
      ),
      silent = TRUE
    )
    
    # If success, break loop
    if (!inherits(out, "try-error")) {
      if (!is.null(out$data) && nrow(out$data) > 0) {
        return(
          out$data |>
            dplyr::mutate(time = as.Date(sub("T12:00:00Z", "", time))) |>
            dplyr::rename(t = time, temp = sst,
                          lon = longitude, lat = latitude) |>
            dplyr::select(lon, lat, t, temp) |>
            tidyr::drop_na()
        )
      }
    }
    
    message("  Attempt ", i, " failed for ", time_df$start,
            " — retrying soon...")
    Sys.sleep(2)
  }

  message("Failed download for: ", time_df$start, " after ", retries, " retries.")
  tibble(lon = NA_real_, lat = NA_real_, t = as.Date(NA), temp = NA_real_)
}



dl_years <- data.frame(date_index = 1:4,
                       start = c("2003-01-01", "2011-01-01", 
                                 "2019-01-01", "2020-01-01"),
                       end = c("2010-12-31", "2018-12-31", 
                               "2019-12-31", "2024-12-31"))


# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
base::system.time(
  OISST_data <- dl_years |> 
    dplyr::group_by(date_index) |> 
    dplyr::group_modify(~OISST_sub_dl(.x)) |> 
    dplyr::ungroup() |> 
    dplyr::select(lon, lat, t, temp)
) # 518 seconds, ~100 seconds per batch


OISST_data |> 
  dplyr::filter(t == "2023-12-01") |> 
  ggplot2::ggplot(aes(x = lon, y = lat)) +
  ggplot2::geom_tile(aes(fill = temp)) +
   #ggplot2::borders() + # Activate this line to see the global map
  ggplot2::scale_fill_viridis_c() +
  ggplot2::coord_quickmap(expand = F) +
  ggplot2::labs(x = NULL, y = NULL, fill = "SST (°C)") +
  ggplot2::theme(legend.position = "bottom")
# just load the shapefile on top of this and see that it overlays, maybe geomsf?

base::saveRDS(OISST_data, file = "C:/Users/luisc/Documents/GitHub/drmr-west-coast/data/OISST_temperature/OISST_vignette.Rds")




#Getting the monthly average of the data

library(data.table)

OISST_vignette_dt <- as.data.table(OISST_vignette)

OISST_vignette_dt[, `:=`(
  year = year(t),
  month = month(t)
)]

monthly_avg <- OISST_vignette_dt[, .(
  mean_temp = mean(temp, na.rm = TRUE),
  n_days = .N
), by = .(lat, lon, year, month)]


lat_band_monthly <- monthly_avg %>%
  group_by(lat, year, month) %>%
  summarise(
    stripe_temp = mean(mean_temp, na.rm = TRUE),
    n_points = n(),
    .groups = "drop"
  )

lat_band_monthly <- lat_band_monthly %>%
  filter(lat >= 32, lat <= 42)

library(dplyr)
library(lubridate)

oisst_temp <- lat_band_monthly %>%
  filter(lat >= 32, lat <= 42) %>%
    mutate(
    group = case_when(
      lat >= 32    & lat < 34.52 ~ "1",
      lat >= 34.52 & lat < 36.75 ~ "2",
      lat >= 36.75 & lat < 38    ~ "3",
      lat >= 38    & lat <= 42   ~ "4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(group, year, month) %>%
  summarise(
    mean_temp = mean(stripe_temp, na.rm = TRUE),
    n_points  = n(),
    .groups   = "drop"
  )

yearly_oisst_temp <- oisst_temp %>%
  group_by(group, year) %>%
  summarise(
    yearly_mean_temp = mean(mean_temp, na.rm = TRUE),   # mean of monthly temps
    yearly_max_temp  = max(mean_temp, na.rm = TRUE),    # hottest monthly mean
    yearly_min_temp  = min(mean_temp, na.rm = TRUE),    # coldest monthly mean
    .groups = "drop"
  )
yearly_oisst_temp <- yearly_oisst_temp %>%
  rename(Year = year)
yearly_oisst_temp <- yearly_oisst_temp %>%
  rename(patch = group)
yearly_oisst_temp <- yearly_oisst_temp %>%
  mutate(patch = as.numeric(patch))

dat_train <- dat_train %>%
  mutate(yearly_mean_temp = as.numeric(yearly_mean_temp))
dat_test <- dat_test %>%
  mutate(yearly_mean_temp = as.numeric(yearly_mean_temp))

dat_train <- dat_train %>%
  select(-c(mean_month_bt, max_month_bt, min_month_bt)) %>%
  left_join(yearly_oisst_temp, by = c("patch", "Year"))


dat_test <- dat_test %>%
  select(-c(mean_month_bt, max_month_bt, min_month_bt)) %>%
  left_join(yearly_oisst_temp, by = c("patch", "Year"))