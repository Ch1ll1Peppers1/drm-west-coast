# Define training/testing split (last 4 years = test)
first_year_forecast <- max(sum_catch$Year) - 4

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

dat_train <- dat_train |> 
  mutate(
    c_btm_temp = yearly_mean_temp - averages["yearly_mean_temp"],
    c_dens_hauls = y - averages["y"],
    time = Year - min(Year)
  )

dat_test <- dat_test |> 
  mutate(
    c_btm_temp = yearly_mean_temp - averages["yearly_mean_temp"],
    c_dens_hauls = y - averages["y"],
    time = Year - first_year_forecast
  )


baseline2 <- fit_drm(
  .data = dat_train,
  y_col = "y",
  time_col = "Year",
  site_col = "patch",
  seed = 202505,
  formula_zero = ~ 1 + c_n_hauls + c_mean_temp + c_max_temp,
  formula_rec  = ~ 1 + c_mean_temp + I(c_min_temp * c_mean_temp),
  init = "cmdstan_default",
)


temp <- tidybayes:: spread_draws(baseline2$stanfit, y_pp[year])

temp <- temp %>%
  mutate(year = year + min(dat_train$Year) - 1) %>%
  filter(year >= first_year, year <= last_year)

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
