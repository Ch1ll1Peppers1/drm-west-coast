# ───────────────────────────────────────────────
#ADDED MORE DATA(2019, 2021–2024)
# ───────────────────────────────────────────────

catch_data_2019 <- download_catch_rates("Sebastes jordani", survey = "WCGBTS_2019", add_zeros = TRUE)
catch_data_2021_2024 <- download_catch_rates("Sebastes jordani", survey = "WCGBTS_2021-2024", add_zeros = TRUE)

# Convert weights to numeric
catch_data_2019$Wt <- as.numeric(catch_data_2019$Wt)
catch_data_2021_2024$Wt <- as.numeric(catch_data_2021_2024$Wt)

# Standardize variable names
sebdata_2019 <- catch_data_2019 |> 
  select(TowID, Lat, Long, Year, Wt) |>
  rename(haul_id = TowID, latitude = Lat, longitude = Long, year = Year, wgt = Wt)

sebdata_21_24 <- catch_data_2021_2024 |> 
  select(TowID, Lat, Long, Year, Wt) |>
  rename(haul_id = TowID, latitude = Lat, longitude = Long, year = Year, wgt = Wt)

# Combine and summarize
sebdata_post18 <- rbind(sebdata_2019, sebdata_21_24) |>
  mutate(density_num = 0)

sebdata_summary <- sebdata_post18 |> 
  group_by(year) |> 
  summarise(density_wgt = mean(wgt, na.rm = TRUE))

sebdata_summary <- sebdata_summary |>
  rename(density = density_wgt)

sebdat_summary <- sebdat_summary |>
  rename(density = density_num)

# Merge all density data
sebdata_final <- rbind(sebdat_summary, sebdata_summary)

# Optional: save intermediate results
# write_csv(sebdata_final, here("data", "seb_by_year.csv"))
# write_csv(hauldat, here("data", "haul_info.csv"))
