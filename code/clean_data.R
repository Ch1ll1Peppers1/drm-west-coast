library(tidyverse)
library(here)

load(here("data","WCANN_clean.RData")) # note there is a readme with column metadata

colnames(data)

# make separate df of haul-level information, so we don't have to track it through joins below 
hauldat <- data |> 
  select(haul_id, latitude, longitude, year, depth) |> 
  distinct()

# add in zeros to get an accurate average num_cpua 
dat_expanded <- expand.grid(year = unique(data$year), haul_id = unique(data$haul_id))

# filter to spp of interest 
sebdat <- data |> 
  filter(accepted_name=="Sebastes jordani") |> 
  select(year, haul_id, num_cpua, wgt_cpua)

# populate the zero-inflated dataframe 
seb_dat_expanded <- dat_expanded |> 
  left_join(sebdat, by=c("year","haul_id")) |> 
  mutate(num_cpua = replace_na(num_cpua, 0))

# average within each year 
sebdat_summary <- seb_dat_expanded |> 
  group_by(year) |> 
  summarise(density_num = mean(num_cpua))


# Add the data for 2019 (download_catch_rates)
catch_data_2019 <- download_catch_rates("Sebastes jordani", survey = "WCGBTS_2019", add_zeros=TRUE)

# Add the data for 2021-2024 (download_catch_rates)
catch_data_2021_2024 <- download_catch_rates("Sebastes jordani", survey = "WCGBTS_2021-2024", add_zeros=TRUE)

#Covert to string
catch_data_2019$Wt <- sapply(catch_data_2019$Wt, toString)
catch_data_2021_2024$Wt <- sapply(catch_data_2021_2024$Wt, toString)

#Choose data necessary and kepe the same naming convention
sebdata_2019 <- catch_data_2019 |> 
  select(TowID, Lat, Long, Year, Wt) |>
  rename(
    haul_id = TowID,
    latitude = Lat,
    longitude = Long,
    year = Year,
    wgt = Wt
  )
sebdata_21_24 <- catch_data_2021_2024 |> 
  select(TowID, Lat, Long, Year, Wt) |>
  rename(
    haul_id = TowID,
    latitude = Lat,
    longitude = Long,
    year = Year,
    wgt = Wt
  )
#Combines data together
sebdata_post18 <- rbind(sebdata_2019, sebdata_21_24)

# average within each year post 18
sebdata_post18$wgt <- as.numeric(sebdata_post18$wgt)
sebdata_post18$density_num <- 0
sebdata_summary <- sebdata_post18 |> 
  group_by(year) |> 
  summarise(
    density_wgt = mean(wgt))

#Combines final data together
sebdata_final <- rbind(sebdat_summary, sebdata_summary)

write_csv(sebdata_final, file=here("data","seb_by_year.csv"))
write_csv(hauldat, file=here("data", "haul_info.csv"))
write_csv(catch_data_2019, file=here("data", "catch_data_2019.csv"))
write_csv(catch_data_2021_2024, file=here("data", "catch_data_2021_2024.csv"))
