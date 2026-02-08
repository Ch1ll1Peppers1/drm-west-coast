#########################################################
# Shortbelly Rockfish DRM Model Creating
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

library(drmr)
library(tidybayes)
library(dplyr)
library(bayesplot)
library(ggplot2)

first_year_forecast <- max(summary_catch_base$Year) - 4

dat_train <- summary_catch_base |> filter(Year < first_year_forecast)
dat_test  <- summary_catch_base |> filter(Year >= first_year_forecast)

# Center covariates
averages <- c(
  yearly_mean_temp = mean(dat_train$yearly_mean_temp, na.rm = TRUE),
  y   = mean(dat_train$y, na.rm = TRUE)
)

cov_means <- dat_train |>
  summarise(
    mean_temp = mean(yearly_mean_temp, na.rm = TRUE),
    max_temp  = mean(yearly_max_temp,  na.rm = TRUE),
    min_temp  = mean(yearly_min_temp,  na.rm = TRUE),
    n_hauls   = mean(n_hauls, na.rm = TRUE)
  )

dat_train <- dat_train |>
  mutate(
    c_mean_temp = yearly_mean_temp - cov_means$mean_temp,
    c_max_temp  = yearly_max_temp  - cov_means$max_temp,
    c_min_temp  = yearly_min_temp  - cov_means$min_temp,
    c_n_hauls   = n_hauls           - cov_means$n_hauls
  )

cov_means <- dat_test |>
  summarise(
    mean_temp = mean(yearly_mean_temp, na.rm = TRUE),
    max_temp  = mean(yearly_max_temp,  na.rm = TRUE),
    min_temp  = mean(yearly_min_temp,  na.rm = TRUE),
    n_hauls   = mean(n_hauls, na.rm = TRUE)
  )

dat_test <- dat_test |>
  mutate(
    c_mean_temp = yearly_mean_temp - cov_means$mean_temp,
    c_max_temp  = yearly_max_temp  - cov_means$max_temp,
    c_min_temp  = yearly_min_temp  - cov_means$min_temp,
    c_n_hauls   = n_hauls           - cov_means$n_hauls
  )

dat_train <- dat_train %>%
  mutate(yearly_mean_temp = as.numeric(yearly_mean_temp))
dat_test <- dat_test %>%
  mutate(yearly_mean_temp = as.numeric(yearly_mean_temp))

dat_train <- dat_train |> 
  mutate(
    c_btm_temp = yearly_mean_temp - averages["yearly_mean_temp"],
    time = Year - min(Year)
  )

dat_test <- dat_test |> 
  mutate(
    c_btm_temp = yearly_mean_temp - averages["yearly_mean_temp"],
    time = Year - first_year_forecast
  )

#--------------------------------------------------------

baseline_1 <- fit_drm(
    .data = dat_train,
    y_col    = "y",
    time_col = "Year",
    site_col = "patch",
    seed = 202505)

temp_0 <- gather_draws(baseline_1$stanfit, y_pp[i]) %>%
  rename(obs = i, y_pp = .value) %>%
  left_join(dat_train %>% mutate(obs = row_number()),
            by = "obs")

plot_out <- temp_0 %>%
  ggplot(aes(x = Year, y = y_pp)) +
  stat_lineribbon() +
  geom_point(
    data = dat_train,
    aes(x = Year, y = y),
    color = "red"
  ) +
  facet_wrap(~patch) +
  labs(x = "Year", y = "Density", title = "baseline_1") +
  scale_fill_brewer() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8))
print(plot_out)

#--------------------------------------------------------

drm_1 <- fit_drm(
  .data = dat_train,
  y_col = "y",
  time_col = "Year",
  site_col = "patch",
  seed = 202505,
  formula_zero = ~ 1 + c_n_hauls + c_mean_temp + c_max_temp,
  formula_rec  = ~ 1 + c_mean_temp + I(c_mean_temp * c_mean_temp),
  init = "cmdstan_default",
)

temp_1 <- gather_draws(drm_1$stanfit, y_pp[i]) %>%
  rename(obs = i, y_pp = .value) %>%
  left_join(dat_train %>% mutate(obs = row_number()),
            by = "obs")


plot_out <- temp_1 %>%
  ggplot(aes(x = Year, y = y_pp)) +
  stat_lineribbon() +
  geom_point(
    data = dat_train,
    aes(x = Year, y = y),
    color = "red"
  ) +
  facet_wrap(~patch) +
  labs(x = "Year", y = "Density", title = "drm_1") +
  scale_fill_brewer() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8))
print(plot_out)

#----------------------------------------------------

drm_2 <-fit_drm(
  .data = dat_train,
  y_col = "y",
  time_col = "Year",
  site_col = "patch",
  seed = 202505,
  formula_zero = ~ 1 + c_n_hauls + c_btm_temp + c_btm_temp,
  formula_rec = ~ 1 + c_btm_temp + c_btm_temp * c_btm_temp,
  formula_surv = ~ 1,
  m = 0,
  .toggles = list(est_surv = 1)
  )


temp_2 <- gather_draws(drm_2$stanfit, y_pp[i]) %>%
  rename(obs = i, y_pp = .value) %>%
  left_join(dat_train %>% mutate(obs = row_number()),
            by = "obs")


plot_out <- temp_2 %>%
  ggplot(aes(x = Year, y = y_pp)) +
  stat_lineribbon() +
  geom_point(
    data = dat_train,
    aes(x = Year, y = y),
    color = "red"
  ) +
  facet_wrap(~patch) +
  labs(x = "Year", y = "Density", title = "drm_2") +
  scale_fill_brewer() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8))
print(plot_out)


#--------------------------------------------------------------

drm_3 <-
  fit_drm(
    .data = dat_train,
    y_col = "y",
    time_col = "Year",
    site_col = "patch",
    seed = 202505,
    formula_zero = ~ 1 + c_n_hauls + c_btm_temp + c_btm_temp,
    formula_rec =  ~ 1 + c_mean_temp + I(c_min_temp * c_mean_temp),
    formula_surv = ~ 1,
    .toggles = list(est_surv = 1,
    ar_re = "rec"))

temp_3 <- gather_draws(drm_3$stanfit, y_pp[i]) %>%
  rename(obs = i, y_pp = .value) %>%
  left_join(dat_train %>% mutate(obs = row_number()),
            by = "obs")

plot_out <- temp_3 %>%
  ggplot(aes(x = Year, y = y_pp)) +
  stat_lineribbon() +
  geom_point(
    data = dat_train,
    aes(x = Year, y = y),
    color = "red"
  ) +
  facet_wrap(~patch) +
  labs(x = "Year", y = "Density", title = "drm_3") +
  scale_fill_brewer() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8))
print(plot_out)

#-------------------------------------------------------

drm_4 <-
  fit_drm(
    .data = dat_train,
    y_col = "y",
    time_col = "Year", ## vector of time points
    site_col = "patch",
    family = "gamma",
    seed = 202505,
    formula_zero = ~ 1 + c_n_hauls + c_btm_temp + c_btm_temp,
    formula_rec = ~ 1 + c_mean_temp + I(c_min_temp * c_mean_temp),
    formula_surv = ~ 1,
    n_ages = 6,
    adj_mat = adj_mat4, ## A matrix for movement routine
    ages_movement = c(0, 0, 1, 1, 1, 0), ## ages allowed to move
    .toggles = list(est_surv = 1,
      ar_re = "rec",
      movement = 1))

temp_4 <- gather_draws(drm_4$stanfit, y_pp[i]) %>%
  rename(obs = i, y_pp = .value) %>%
  left_join(dat_train %>% mutate(obs = row_number()),
            by = "obs")

plot_out <- temp_4 %>%
  ggplot(aes(x = Year, y = y_pp)) +
  stat_lineribbon() +
  geom_point(
    data = dat_train,
    aes(x = Year, y = y),
    color = "red"
  ) +
  facet_wrap(~patch) +
  labs(x = "Year", y = "Density", title = "drm_4") +
  scale_fill_brewer() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8))
print(plot_out)

#-------------------------------------------

drm_5 <-
  fit_drm(
    .data = dat_train,
    y_col = "y",
    time_col = "Year",
    site_col = "patch",
    family = "gamma",
    seed = 202505,
    formula_zero = ~ 1 + c_n_hauls + c_btm_temp + c_btm_temp,
    formula_rec = ~ 1 + c_mean_temp + I(c_min_temp * c_mean_temp),
    formula_surv = ~ 1,
    n_ages = 3,
    adj_mat = adj_mat4,
    ages_movement = c(0, 0, 1),
    .toggles = list(est_surv = 1,
                    ar_re = "rec",
                    movement = 1))

temp_5 <- gather_draws(drm_5$stanfit, y_pp[i]) %>%
  rename(obs = i, y_pp = .value) %>%
  left_join(dat_train %>% mutate(obs = row_number()),
            by = "obs")

plot_out <- temp_5 %>%
  ggplot(aes(x = Year, y = y_pp)) +
  stat_lineribbon() +
  geom_point(
    data = dat_train,
    aes(x = Year, y = y),
    color = "red"
  ) +
  facet_wrap(~patch) +
  labs(x = "Year", y = "Density", title = "drm_5") +
  scale_fill_brewer() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8))
print(plot_out)
