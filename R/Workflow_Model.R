#########################################################
# Shortbelly Rockfish DRM Creation Workflow
#
# Author: Luis Mario Ramirez Cruz
#
# Last time modified: 10/11/2025
#
# Any current bugs (Y/N)? N
#
# Description:
#   This worflow will help you download, clean and merge shortbelly
#   rockfish data from multiple sources, it also adds environmental 
#   covariates (bottom temperature), and fits dynamic rate models (DRMs) 
#   to explore population trends and environmental effects.
#
# Any missing data or note will begin with **
#
# Currently working in:Adding bio data
#
#
###################################################################

# ───────────────────────────────────────────────
# SETTING UP THE ENVIRONMENT
# ───────────────────────────────────────────────

# Uncomment to install necessary packages if you haven't installed them yet
# install.packages(c("tidyverse", "here", "remotes"))
# remotes::install_github("pfmc-assessments/nwfscSurvey")
# remotes::install_github("afredston/drmr-west-coast")

library(tidyverse)
library(tidybase)
library(here)
library(nwfscSurvey)
library(drmr)
library(shinystan)
library(bayesplot)

# ───────────────────────────────────────────────
# DOWNLOADING NWSFC COMBO DATA (2003–2024)
# ───────────────────────────────────────────────

catch <- pull_catch(
  common_name = "shortbelly rockfish",
  years = c(2003, 2024),
  survey = "NWFSC.Combo",
  verbose = TRUE
)
catch <- catch[catch$Latitude_dd >= 32, ] #Only the area of California
catch <- catch[catch$Latitude_dd <= 42, ] #Only the area of California


summary_catch <- catch |> 
  select(Trawl_id, Latitude_dd, Longitude_dd, Year, total_catch_numbers, Area_swept_ha) |> 
  distinct()

# Standardize density to catches per km²
summary_catch <- summary_catch |> 
  mutate(total_catches_per_km2 = (total_catch_numbers / Area_swept_ha) * 100)

# Summarize annual densities
sebastis_jordani_data <- summary_catch |> 
  group_by(Year) |> 
  summarise(
    density_num = mean(total_catches_per_km2),
    sum_num = sum(total_catches_per_km2)
  )

# ───────────────────────────────────────────────
# ADDING BTM TEMPERATURE (COVARIATE) **ONLY HAVE BTM TEMP UNTIL 2019 
# ───────────────────────────────────────────────

btm_temp <- read.csv(here("data", "glorys_wcann_bt_for_lmrc.csv"))
btm_temp[nrow(btm_temp) + 1, ] <- NA  # Adding NA because we do not have data for 2020 (COVID)


#Now double check how many rows each has
n_btm <- nrow(btm_temp)
n_summ <- nrow(sebastis_jordani_data)

# If shorter, pad with NA rows to match sebastis_jordani_data
if (n_btm < n_summ) {
  n_missing <- n_summ - n_btm
  padding <- as.data.frame(matrix(NA, nrow = n_missing, ncol = ncol(btm_temp)))
  names(padding) <- names(btm_temp)
  btm_temp <- rbind(btm_temp, padding)
}

sebastis_jordani_data <- sebastis_jordani_data |>
  mutate(
    mean_btm_temp = btm_temp$mean_month_bt,
    max_btm_temp  = btm_temp$max_month_bt,
    min_btm_temp  = btm_temp$min_month_bt
  )

# Optional: Save summary for safe keeping
# write_csv(sebastis_jordani_data, file = here("data", "sebastis_jordani_data.csv"))

# ───────────────────────────────────────────────
# HISTORICAL HAUL DATA
# ───────────────────────────────────────────────

load(here("data","WCANN_clean.RData"))  # pre-cleaned data

hauldat <- data |> 
  select(haul_id, latitude, longitude, year, depth) |> 
  distinct()

# Add zero-inflated structure
dat_expanded <- expand.grid(year = unique(data$year), haul_id = unique(data$haul_id))

# Filter shortbelly rockfish records
sebdat <- data |> 
  filter(accepted_name == "Sebastes jordani") |> 
  select(year, haul_id, num_cpua, wgt_cpua)

# Merge and fill missing (zero) records
seb_dat_expanded <- dat_expanded |> 
  left_join(sebdat, by = c("year", "haul_id")) |> 
  mutate(num_cpua = replace_na(num_cpua, 0))

# Average by year
sebdat_summary <- seb_dat_expanded |> 
  group_by(year) |> 
  summarise(density_num = mean(num_cpua))

# ───────────────────────────────────────────────
# ** IN PROGRESS OBTAIN BIO DATA
# ───────────────────────────────────────────────


bio <- pull_bio(
  common_name = "shortbelly rockfish",
  years = c(2003, 2024),
  survey = "NWFSC.Combo",
  verbose = TRUE
)

bio <- bio[bio$Latitude_dd >= 32, ] #Only the area of California
bio <- bio[bio$Latitude_dd <= 42, ] #Only the area of California


strata <- CreateStrataDF.fn(
  names = c("shallow_s", "deep_s", "shallow_n", "deep_n"), 
  depths.shallow = c(  55,  183,  55, 183),
  depths.deep    = c( 183,  400, 183, 400),
  lats.south     = c(  32, 32,  36.75,  36.75),
  lats.north     = c(  36.75, 36.75,  42,  42)
)

hist(bio$Length_cm,
     main = "Distribution in cm",
     xlab = "Length_cm",
     col = "skyblue",
     border = "white")

hist(bio$Age_years,
     main = "Distribution in cm",
     xlab = "Age",
     col = "darkgreen",
     border = "white")


length_comps <- get_expanded_comps(
  bio_data = bio,
  catch_data = catch,
  comp_bins = seq(13, 30, 2),
  strata = strata,
  comp_column_name = "length_cm",
  output = "full_expansion_ss3_format",
  two_sex_comps = TRUE,
  input_n_method = "stewart_hamel")

plot_comps(
  data = length_comps)

age_comps <- get_expanded_comps(
  bio_data = bio,
  catch_data = catch,
  comp_bins = (1:20),
  strata = strata,
  comp_column_name = "age",
  output = "full_expansion_ss3_format",
  two_sex_comps = TRUE,
  input_n_method = "stewart_hamel")

plot_comps(
  data = age_comps)

PlotMap.fn(
  dat = catch,
  plot = 1)

# ───────────────────────────────────────────────
# ADDING AREA IN KM^2 (LOOK AT DATA MAKER) AND ADD DATA MAKER **
# ───────────────────────────────────────────────

patch_areas <- data.frame(
  patch = c(1, 2, 3, 4),
  area_km2 = c(61238.287611, 18133.043869, 10286.465331, 27234.993392)  # example values
)

# Add new column to df
summary_merged <- summary_merged %>%
  left_join(patch_areas, by = "patch")


# ───────────────────────────────────────────────
# PREPARING MAP FOR MODELLING
# ───────────────────────────────────────────────

map2patches <- st_read("data/Cali_2Patches.shp")
map4patches <- st_read("data/Cali_4Patches.shp")

map2patches |>
  st_area() |>
  units::set_units("km^2") |>
  summary()

map4patches |>
  st_area() |>
  units::set_units("km^2") |>
  summary()

adj_mat2 <- gen_adj(st_buffer(st_geometry(map2patches),
                             dist = 2500))
adj_mat4 <- gen_adj(st_buffer(st_geometry(map4patches),
                              dist = 2500))

## row-standardized matrix
adj_mat2 <-
  t(apply(adj_mat2, 1, \(x) x / (sum(x))))

adj_mat4 <-
  t(apply(adj_mat4, 1, \(x) x / (sum(x))))

# ───────────────────────────────────────────────
# PREPARE DATA FOR MODELLING
# ───────────────────────────────────────────────

sebastis_jordani_data$patch <- 1  # single patch
sebastis_jordani_data$patch <- as.numeric(sebastis_jordani_data$patch)

# Define training/testing split (last 4 years = test)
first_year_forecast <- max(sebastis_jordani_data$Year) - 4

dat_train <- sebastis_jordani_data |> filter(Year < first_year_forecast)
dat_test  <- sebastis_jordani_data |> filter(Year >= first_year_forecast)

# Center covariates
averages <- c(
  mean_btm_temp = mean(dat_train$mean_btm_temp, na.rm = TRUE),
  density_num   = mean(dat_train$density_num, na.rm = TRUE)
)

dat_train <- dat_train |> 
  mutate(
    c_btm_temp = mean_btm_temp - averages["mean_btm_temp"],
    c_dens_hauls = density_num - averages["density_num"],
    time = Year - min(Year)
  )

dat_test <- dat_test |> 
  mutate(
    c_btm_temp = mean_btm_temp - averages["mean_btm_temp"],
    c_dens_hauls = density_num - averages["density_num"],
    time = Year - first_year_forecast
  )

# ───────────────────────────────────────────────
# FITTING DRMS 
# ───────────────────────────────────────────────

# Baseline model (no covariates)
baseline <- fit_drm(
  .data = dat_train,
  y_col = "density_num",
  time_col = "Year",
  site_col = "patch",
  seed = 202505
)


# Model with temperature effect on survival
drm_1_surv <- fit_drm(
  .data = dat_train,
  y_col = "y",
  time_col = "time",
  site_col = "patch",
  formula_surv = ~ 1 + yearly_mean_temp + I(yearly_max_temp^2),
  n_ages = 2,
  seed = 222505
)

# Model with temperature effect on recruitment
drm_1_rec <- fit_drm(
  .data = dat_train,
  y_col = "density_num",
  time_col = "time",
  site_col = "patch",
  formula_rec = ~ 1 + mean_btm_temp + I(mean_btm_temp^2),
  n_ages = 2,
  iter_warmup = 1200,
  iter_sampling = 1200,
  seed = 222505
)

# ───────────────────────────────────────────────
# POST CHECK **
## MIGHT BE NEEDED
temp <- temp %>%
  mutate(year = year + min(dat_train$Year) - 1)
# ───────────────────────────────────────────────

plot_out <- temp %>%
  ggplot(aes(x = year, y = y_pp)) +
  stat_lineribbon() +
  geom_point(
    data = dat_train %>% mutate(year = Year),     # map Year to lowercase year
    aes(x = year, y = y),
    color = "red"
  ) +
  facet_wrap(~patch, scales = "free_y") +
  labs(x = "Year", y = "Density", title = "baseline2") +
  scale_fill_brewer() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8))

print(plot_out)

# ───────────────────────────────────────────────
# POST CHECK (ShinyStan)
# ───────────────────────────────────────────────

library(bayesplot)
library(ggplot2)

# Hide legends globally
theme_set(theme_minimal() + theme(legend.position = "none"))

color_scheme_set(c("#377eb8","#054171", "#BE2745", "#CB7331", "#CB7332", "#999999"))

# Draw your plot
baseline$stanfit$draws(variables = c("phi", "beta_t", "beta_r")) |>
  mcmc_combo(
    combo = c("trace", "dens_overlay"),
    facet_args = list(NULL)
  )


baseline$stanfit$draws(variables = c("phi",
                                         "beta_t",
                                         "beta_r")) |>
  mcmc_combo(combo = c("trace", "dens_overlay"))


sdrm_1 <- as.shinystan(baseline$stanfit)
launch_shinystan(sdrm_1)
