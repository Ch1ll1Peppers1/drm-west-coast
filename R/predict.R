##' @inherit get_fitted_pars
fitted_pars_drm <- function(data_list) {
  output <- c("beta_t", "beta_r",
              "lambda")
  if (data_list$rho_mu == 1)
    output <- c(output, "xi")
  if (data_list$likelihood == 0) {
    output <- c(output, "sigma_obs")
  } else {
    output <- c(output, "phi")
  }
  if (data_list$movement == 1)
    output <- c(output, "mov_mat")
  if (data_list$est_surv == 1)
    output <- c(output, "beta_s")
  if (data_list$ar_re > 0)
    output <- c(output, "z_t", "alpha", "sigma_t")
  if (data_list$iid_re > 0)
    output <- c(output, "z_i", "sigma_i")
  if (data_list$sp_re > 0)
    output <- c(output, "z_s")
  return(output)
}

##' @inherit get_fitted_pars
fitted_pars_sdm <- function(data_list) {
  output <- c("beta_t", "beta_r")
  if (data_list$rho_mu == 1)
    output <- c(output, "xi")
  if (data_list$likelihood == 0) {
    output <- c(output, "sigma_obs")
  } else {
    output <- c(output, "phi")
  }
  if (data_list$ar_re == 1)
    output <- c(output, "z_t", "alpha", "sigma_t")
  if (data_list$iid_re == 1)
    output <- c(output, "z_i")
  if (data_list$sp_re == 1)
    output <- c(output, "z_s")
  return(output)
}

##' @title Retrieve parameters needed for forecasting
##'
##' @description This function identifies the parameters necessary for carrying
##'   out forecasts based on the data used to fit a DRM (or SDM).
##'
##' @param data_list a \code{list} used as input for model fitting. Typically,
##'   the output from the [make_data] function.
##' @param model a \code{character} indicating which model forecasts are sought
##'   for. This input admits two possible entries: "drm" (default) or "sdm".
##' @return a \code{character} vector of labels indicating the parameters
##'   necessary for the forecast.
##' @author lcgodoy
get_fitted_pars <- function(data_list, model = "drm") {
  stopifnot(inherits(data_list, "list"))
  stopifnot(length(model) == 1)
  if (model == "drm") {
    output <- fitted_pars_drm(data_list)
  } else
    output <- fitted_pars_sdm(data_list)
  return(output)
}

##' @title Forecasts based on DRM.
##'
##' @description Considering a new dataset (across the same patches), computes
##'   forecasts based on the DRM passed as \code{drm}.
##'
##' @param drm A \code{list} object containing the output from the [fit_drm]
##'   function.
##' @param new_data a \code{data.frame} with the dataset at which we wish to
##'   obtain predictions.
##' @param past_data a \code{data.frame} with the dataset last year used in
##'   model fitting. Only needed when \code{f_test} is not missing or when
##'   estimating survival.
##' @param f_test a \code{matrix} informing the instantaneous fishing mortality
##'   rates at each age (columns) and timepoint (rows).
##' @param seed a seed used for the forecasts. Forecasts are obtained through
##'   Monte Carlo samples from the posterior predictive distribution. Therefore,
##'   a \code{seed} is needed to ensure the results' reproducibility.
##' @param cores number of threads used for the forecast. If four chains were
##'   used in the \code{drm}, then four (or less) threads are recommended.
##'
##'
##' @details The current version of the code assumes the data where forecasts
##'   are needed is ordered by "patch" and "site" and, in addition, its patches
##'   MUST be the same as the ones used to obtain the parameters' estimates from
##'   the the \code{drm} object.
##'
##' @author lcgodoy
##'
##' @return an object of class \code{"CmdStanGQ"} containing samples for the
##'   posterior predictive distribution for forecasting.
##'
##' @export
predict_drm <- function(drm,
                        new_data,
                        past_data,
                        f_test,
                        seed = 1,
                        cores = 1) {
  stopifnot(inherits(drm$stanfit, "CmdStanFit"))
  ## number time points for forecasting
  ntime_for <- length(unique(new_data[[drm$cols$time_col]]))
  time_for <- new_data[[drm$cols$time_col]] -
    min(new_data[[drm$cols$time_col]]) + 1
  if (!missing(f_test)) {
    stopifnot(NROW(f_test) == drm$data[["n_ages"]])
  }
  x_tt <- stats::model.matrix(drm[["formulas"]][["formula_zero"]],
                              data = new_data)
  x_rt <- stats::model.matrix(drm[["formulas"]][["formula_rec"]],
                              data = new_data)
  if (missing(f_test)) {
    f_test <- matrix(0, ncol = ntime_for, nrow = drm$data$n_ages)
  }
  ##--- pars from model fitted ----
  pars <- get_fitted_pars(drm$data, "drm")
  fitted_params <-
    drm$stanfit$draws(variables = pars)
  ##--- list for forecast object ----
  forecast_data <-
    list(n_patches = drm$data$n_patches,
         n_ages = drm$data$n_ages,
         n_time = ntime_for,
         n_time_train = drm$data$n_time,
         time = time_for,
         patch = new_data[[drm$cols$site_col]],
         rho_mu = drm$data$rho_mu,
         ar_re = drm$data$ar_re,
         iid_re = drm$data$iid_re,
         sp_re = drm$data$sp_re,
         movement = drm$data$movement,
         est_surv = drm$data$est_surv,
         cloglog = drm$data$cloglog,
         likelihood = drm$data$likelihood,
         K_t = drm$data$K_t,
         X_t = x_tt,
         f = f_test,
         f_past = drm$data$f,
         m = drm$data$m,
         ages_movement = drm$data$ages_movement,
         selectivity_at_age = drm$data$selectivity_at_age,
         K_r = drm$data$K_r,
         X_r = x_rt)
  forecast_data$N <- forecast_data$n_patches * forecast_data$n_time
  if (length(drm$data$K_m) > 0) {
    stopifnot(!missing(past_data))
    x_mpast <-
      stats::model.matrix(drm[["formulas"]][["formula_surv"]],
                          data = past_data)
    x_m <- stats::model.matrix(drm[["formulas"]][["formula_surv"]],
                               data = new_data)
    forecast_data$K_m <- array(NCOL(x_m), dim = 1)
    forecast_data$X_m <- x_m
    forecast_data$X_m_past <- x_mpast
  } else {
    forecast_data$K_m <- integer(0)
    forecast_data$X_m <- matrix(0)
    forecast_data$X_m_past <- matrix(0)
  }
  ## load compiled forecast
  forecast_comp <-
    instantiate::stan_package_model(name = "forecast",
                                    package = "drmr")
  ## computing forecast
  output <- forecast_comp$
    generate_quantities(fitted_params = fitted_params,
                        data = forecast_data,
                        seed = seed,
                        parallel_chains = cores)
  return(output)
}

##' @title Forecasts based on SDM.
##'
##' @description Considering a new dataset (across the same patches), computes
##'   forecasts based on the SDM passed as \code{sdm}.
##'
##' @description Consider a linear predictor having linear and square terms
##'   associated with a variable \eqn{x}. Assume this variable was centered
##'   before being included in the linear predictor. This functions returns the
##'   value of \eqn{x} (on its original scale) such that the linear predictor is
##'   maximized (or minimized).
##'
##' @param sdm A \code{list} object containing the output of a [fit_sdm] call.
##'
##' @param new_data a \code{data.frame} with the dataset at which we wish to
##'   obtain predictions.
##' @param seed a seed used for the forecasts. Forecasts are obtained through
##'   Monte Carlo samples from the posterior predictive distribution. Therefore,
##'   a \code{seed} is needed to ensure the results' reproducibility.
##' @param cores number of threads used for the forecast. If four chains were
##'   used in the \code{drm}, then four (or less) threads are recommended.
##'
##' @details The current version of the code assumes the data where forecasts
##'   are needed is ordered by "patch" and "site" and, in addition, its patches
##'   MUST be the same as the ones used to obtain the parameters' estimates from
##'   the the \code{sdm} object.
##'
##' @author lcgodoy
##'
##' @return an object of class \code{"CmdStanGQ"} containing samples for the
##'   posterior predictive distribution for forecasting.
##'
##' @export
predict_sdm <- function(sdm,
                        new_data,
                        seed = 1,
                        cores = 1) {
  stopifnot(inherits(sdm$stanfit, "CmdStanFit"))
  ## time points for forecasting
  ntime_for <- length(unique(new_data[[sdm$cols$time_col]]))
  time_for <- new_data[[sdm$cols$time_col]] -
    min(new_data[[sdm$cols$time_col]]) + 1
  ##--- pars from model fitted ----
  pars <- get_fitted_pars(sdm$data, "sdm")
  fitted_params <-
    sdm$stanfit$draws(variables = pars)
  ##--- list for forecast object ----
  forecast_data <-
    list(n_patches = sdm$data$n_patches,
         n_time = ntime_for,
         n_time_train = sdm$data$n_time,
         time = time_for,
         patch = new_data[[sdm$cols$site_col]],
         rho_mu = sdm$data$rho_mu,
         ar_re = sdm$data$ar_re,
         iid_re = sdm$data$iid_re,
         sp_re = sdm$data$sp_re,
         cloglog = sdm$data$cloglog,
         likelihood = sdm$data$likelihood,
         Z = stats::model.matrix(sdm[["formulas"]][["formula_zero"]],
                                 data = new_data),
         X = stats::model.matrix(sdm[["formulas"]][["formula_dens"]],
                                 data = new_data))
  forecast_data$N <- forecast_data$n_patches * forecast_data$n_time
  forecast_data$K_z <- NCOL(forecast_data$Z)
  forecast_data$K_x <- NCOL(forecast_data$X)
  ## load compiled forecast
  forecast_comp <-
    instantiate::stan_package_model(name = "forecast_sdm",
                                    package = "drmr")
  ## computing forecast
  output <- forecast_comp$
    generate_quantities(fitted_params = fitted_params,
                        data = forecast_data,
                        seed = seed,
                        parallel_chains = cores)
  return(output)
}
