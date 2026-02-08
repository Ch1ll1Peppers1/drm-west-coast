#install.packages("remotes")
#remotes::install_github("pfmc-assessments/nwfscSurvey")

library(tidyverse)
library(here)
library(nwfscSurvey)
dat <- pull_catch(
  common_name = "shortbelly rockfish",
  years = c(2003, 2024),
  survey = "NWFSC.Combo",
  verbose = TRUE
)

nwsfcdat <- dat |> 
  select(Trawl_id, Latitude_dd, Longitude_dd, Year, total_catch_numbers, Area_swept_ha) |> 
  distinct()

#Standardizing data to km^2
nwsfcdat$total_catches_per_km2 <- (nwsfcdat$total_catch_numbers / nwsfcdat$Area_swept_ha) * 100


nwsfcdat_summary <- nwsfcdat |> 
  group_by(Year) |> 
  summarise(
    density_num = mean(total_catches_per_km2),
    sum_num = sum(total_catches_per_km2))


#Adding the month bottom temperature data to our summary
btm_temp <- read.csv("C:/Users/luisc/Documents/GitHub/drmr-west-coast/code/glorys_wcann_bt_for_lmrc.csv")

btm_temp[nrow(btm_temp) + 1, ] <- 0

nwsfcdat_summary$mean_btm_temp <- btm_temp$mean_month_bt
nwsfcdat_summary$max_btm_temp <- btm_temp$max_month_bt
nwsfcdat_summary$min_btm_temp <- btm_temp$min_month_bt

#write_csv(nwsfcdat_summary, file=here("data", "data_summary.csv"))