remotes::install_github("afredston/drmr-west-coast")
library(drmr) ## load package
library(shinystan)
data(nwsfcdat_summary)  ## load data

nwsfcdat_summary$patch <- ("1")
nwsfcdat_summary$patch <- as.numeric(nwsfcdat_summary$patch)

first_year_forecast <- max(nwsfcdat_summary$Year) - 4

first_id_forecast <-
  first_year_forecast - min(nwsfcdat_summary$Year) + 1

years_all <- order(unique(nwsfcdat_summary$Year))
years_train <- years_all[years_all < first_id_forecast]
years_test <- years_all[years_all >= first_id_forecast]

dat_test <- nwsfcdat_summary |>
  filter(Year >= first_year_forecast)

dat_train <- nwsfcdat_summary |>
  filter(Year < first_year_forecast)

averages <- c("mean_btm_temp" = mean(dat_train$mean_btm_temp),
              "density_num" = mean(dat_train$density_num)
              )

dat_test <- dat_test |>
  mutate(c_btm_temp = mean_btm_temp - averages[["mean_btm_temp"]],
         c_dens_hauls = density_num - averages[["density_num"]],
         time  = Year - first_year_forecast)
dat_train <- dat_train |>
  mutate(c_btm_temp = mean_btm_temp - averages["mean_btm_temp"],
         c_dens_hauls = density_num - averages["density_num"],
         time  = Year - 2002)


baseline <-
  fit_drm(.data = dat_train,  ## dataset
          y_col = "density_num",     ## response variable
          time_col = "Year",  ## variable storing time
          site_col = "patch", ## variable storing "patch/site" id
          seed = 202505)      ## seed

drm_1_surv <-
  fit_drm(.data = dat_train, ##our dataset
          y_col = "density_num",
          time_col = "time",
          site_col = "patch",
          formula_surv = ~ 1 + mean_btm_temp + I(mean_btm_temp * mean_btm_temp),
          n_ages = 2,
          seed = 222505
          )

drm_1_rec <-
  fit_drm(.data = dat_train, ##our dataset
          y_col = "density_num",
          time_col = "time",
          site_col = "patch",
          formula_rec = ~ 1 + mean_btm_temp + I(mean_btm_temp * mean_btm_temp),
          n_ages = 2,
          iter_warmup = 1200,
          iter_sampling = 1200,
          seed = 222505
  )


sdrm_1 <- as.shinystan(drm_1_rec$stanfit)
launch_shinystan(sdrm_1)


