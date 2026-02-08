# we need to update the WCANN survey to include recent years
# but the QA/QC script in FISHGLOB is written for OceanAdapt, not for the NOAA NWFSC download
# let's go through it and be sure we end up with the same columns 
library(here)
library(tidyverse)
source(here("code","download_catch_rates.R"))
source(here("code","add_missing_zeros.R"))

load(here("data","WCANN_clean.RData")) # note there is a readme with column metadata
fishglob <- data
rm(data)

temp <- tempfile()
download.file(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/wcann_catch.csv.zip", temp)
oceanadapt <- read_csv(unz(temp, "wcann_catch.csv")) |> 
  filter(scientific_name == "Sebastes jordani",
         year == 2018)

noaa_api <- download_catch_rates("Sebastes jordani", survey = "WCGBTS_2018", add_zeros=FALSE) # all NOAA WCGBTS files have area_swept = 1 ha 

# Hard to tell why OceanAdapt --> FISHGLOB (https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_wcann.R#L134) used the column "total_catch_wt_kg" (which we don't have from the NOAA API) vs "cpue_kg_per_ha_der" (could have just multiplied to convert ha to km2, right?) It's also not clear where OceanAdapt got the count data from; the NOAA API just has biomass. 

# let's check surveyjoin, maybe they have the counts? 
library(surveyjoin)
cache_data()
load_sql_data()
d <- get_data("shortbelly rockfish")


pak::pkg_install("DFO-NOAA-Pacific/surveyjoin")
