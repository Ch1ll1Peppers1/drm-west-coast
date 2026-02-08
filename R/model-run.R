##' @title Fit the dynamic range model.
##' @export
##' @family models
##' @description Fit the DRM Stan model (this function is not stable yet and
##'   have not been fully tested).
##' @param .data A \code{data frame} containing the data for the model.
##' @param y_col A \code{character} specifying the name of the column in `.data`
##'   that contains the response variable.
##' @param time_col A \code{character} specifying the name of the column in
##'   `.data` that contains the time variable.
##' @param site_col A \code{character} specifying the name of the column in
##'   `.data` that contains the site variable.
##' @param family a \code{character} specifying the family of the probability
##'   distribution assumed for density. The options are: \itemize{
##'   \item \code{"gamma"} (default): gamma parametrized in terms of its mean;
##'   \item \code{"lognormal"}: log-normal parametrized in terms of its mean;
##'   \item \code{"loglogistic"}: log-logistic parametrized in terms of its mean.
##'   \item \code{"lognormal_legacy"} (default): log-normal with its usual parametrization;
##'    }
##' @param formula_zero A \code{formula} specifying the model for the zero
##'   inflation component. Defaults to `~ 1` (intercept only).
##' @param formula_rec A \code{formula} specifying the model for the recruitment
##'   component. Defaults to `~ 1` (intercept only).
##' @param formula_surv A \code{formula} specifying the model for the survival
##'   component. If `NULL` (the default), no survival component is included.
##' @param iter_warmup An \code{integer} specifying the number of warmup
##'   iterations for the MCMC sampler. Defaults to 1000.
##' @param iter_sampling An \code{integer} specifying the number of sampling
##'   iterations for the MCMC sampler. Defaults to 1000.
##' @param chains An \code{integer} specifying the number of MCMC chains.
##'   Defaults to 4.
##' @param parallel_chains An \code{integer} specifying the number of chains to
##'   run in parallel. Defaults to 4.
##' @param seed An \code{integer} specifying the random number seed.
##' @param init A scalar specifying the initialization method. The default
##'   (NULL) represents the \code{cmdstan} default, a scalar greater than zero,
##'   say \code{x}, initialized all parameters from a uniform between \code{-x}
##'   and \code{x}, a \code{0} initialize all parameters at \code{0}, "prior"
##'   (to initialize the model parameters using samples from their prior) or
##'   "pathfinder".
##' @param ... Passed on to the [make_data()] function used to build the input
##'   \code{list} for our \code{cmdstanr} model.
##' @return A \code{list} containing the MCMC draws, the model data, the linear
##'   predictors formulas, and the (response, time, site) column names.
##'   Specifically: \itemize{
##'    \item \code{stanfit}: The MCMC draws from the fitted model.
##'    \item\code{data}: The data used to fit the model (as a list).
##'    \item\code{formulas}: The formulas used to create design matrices.
##'    \item\code{cols}: Important column names.
##'   }
##' @seealso [make_data()]
##' @examples
##' if (instantiate::stan_cmdstan_exists()) {
##'   data(sum_fl)
##'   fit_drm(.data = sum_fl,
##'           y_col = "y",
##'           time_col = "year",
##'           site_col = "patch",
##'           seed = 2025)$stanfit$summary()
##' }
##' @author lcgodoy
fit_drm <- function(.data,
                    y_col,
                    time_col,
                    site_col,
                    family = "gamma",
                    formula_zero = ~ 1,
                    formula_rec = ~ 1,
                    formula_surv = NULL,
                    iter_warmup   = 1000,
                    iter_sampling = 1000,
                    chains = 4,
                    parallel_chains = 4,
                    seed,
                    init = "cmdstan_default",
                    ...) {
  stopifnot(length(init) == 1)
  if (is.character(init))
    stopifnot(init %in% c("cmdstan_default", "pathfinder", "prior"))
  x_t <- stats::model.matrix.lm(formula_zero, data = .data,
                                na.action = stats::na.fail)
  x_r <- stats::model.matrix.lm(formula_rec, data = .data,
                                na.action = stats::na.fail)
  if (is.null(formula_surv)) {
    model_dat <- make_data(y = .data[[y_col]],
                           time = .data[[time_col]],
                           site = .data[[site_col]],
                           family = family,
                           x_t = x_t,
                           x_r = x_r,
                           ...)
  } else {
    x_m <- stats::model.matrix.lm(formula_surv, data = .data,
                                  na.action = stats::na.fail)
    model_dat <- make_data(y = .data[[y_col]],
                           time = .data[[time_col]],
                           site = .data[[site_col]],
                           family = family,
                           x_t = x_t,
                           x_r = x_r,
                           x_m = x_m,
                           ...)
  }
  model <- instantiate::stan_package_model(
                            name = "drm",
                            package = "drmr"
                        )
  if (init == "cmdstan_default") {
    drm_init <- NULL
  } else if (init == "prior") {
    drm_init <-
      prior_inits(model_dat, chains, "drm")
  } else if (init == "pathfinder") {
    drm_init <-
      model$pathfinder(data = model_dat,
                       seed = seed,
                       num_paths = chains,
                       save_single_paths = TRUE,
                       psis_resample = FALSE)
  } else {
    drm_init <- init
  }
  draws <- model$sample(data = model_dat,
                        iter_warmup = iter_warmup,
                        iter_sampling = iter_sampling,
                        seed = seed,
                        chains = chains,
                        parallel_chains = parallel_chains,
                        init = drm_init)
  output <-
    list("stanfit"  = draws,
         "data"     = model_dat,
         "formulas" = list("formula_zero" = formula_zero,
                           "formula_rec" = formula_rec,
                           "formula_surv" = formula_surv),
         "cols" = list("y_col" = y_col,
                       "time_col" = time_col,
                       "site_col" = site_col))

  return(output)
}

##' @title Fit a GLM based SDM
##' @export
##' @family models
##' @description Fit the SDM Stan model (this function is not stable yet and
##'   have not been fully tested).
##' @param .data A \code{data frame} containing the data for the model.
##' @param y_col A \code{character} specifying the name of the column in `.data`
##'   that contains the response variable.
##' @param time_col A \code{character} specifying the name of the column in
##'   `.data` that contains the time variable.
##' @param site_col A \code{character} specifying the name of the column in
##'   `.data` that contains the site variable.
##' @param family a \code{character} specifying the family of the probability
##'   distribution assumed for density. The options are: \itemize{
##'   \item \code{"gamma"} (default): gamma parametrized in terms of its mean;
##'   \item \code{"lognormal"}: log-normal parametrized in terms of its mean;
##'   \item \code{"loglogistic"}: log-logistic parametrized in terms of its mean.
##'   \item \code{"lognormal_legacy"} (default): log-normal with its usual parametrization;
##'    }
##' @param formula_zero A \code{formula} specifying the model for the zero
##'   inflation component. Defaults to `~ 1` (intercept only).
##' @param formula_dens A \code{formula} specifying the model for the non-zero
##'   density component. Defaults to `~ 1` (intercept only).
##' @param iter_warmup An \code{integer} specifying the number of warmup
##'   iterations for the MCMC sampler. Defaults to 1000.
##' @param iter_sampling An \code{integer} specifying the number of sampling
##'   iterations for the MCMC sampler. Defaults to 1000.
##' @param chains An \code{integer} specifying the number of MCMC chains.
##'   Defaults to 4.
##' @param parallel_chains An \code{integer} specifying the number of chains to
##'   run in parallel. Defaults to 4.
##' @param seed An \code{integer} specifying the random number seed.
##' @param init A scalar specifying the initialization method. The default
##'   (NULL) represents the \code{cmdstan} default, a scalar greater than zero,
##'   say \code{x}, initialized all parameters from a uniform between \code{-x}
##'   and \code{x}, a \code{0} initialize all parameters at \code{0}, "prior"
##'   (to initialize the model parameters using samples from their prior) or
##'   "pathfinder".
##' @param ... Passed on to the [make_data()] function used to build the input
##'   \code{list} for our \code{cmdstanr} model.
##' @return A \code{list} containing the MCMC draws, the model data, the linear
##'   predictors formulas, and the (response, time, site) column names.
##'    \itemize{
##'     \item \code{stanfit}: The MCMC draws from the fitted model.
##'     \item\code{data}: The data used to fit the model (as a list).
##'     \item\code{formulas}: The data used to fit the model (as a list).
##'     \item\code{cols}: Important column names.
##'   }
##' @seealso [make_data()]
##' @examples
##' if (instantiate::stan_cmdstan_exists()) {
##'   data(sum_fl)
##'   fit_sdm(.data = sum_fl,
##'           y_col = "y",
##'           time_col = "year",
##'           site_col = "patch",
##'           seed = 2025)$stanfit$summary()
##' }
##' @author lcgodoy
fit_sdm <- function(.data,
                    y_col,
                    time_col,
                    site_col,
                    family = "gamma",
                    formula_zero = ~ 1,
                    formula_dens = ~ 1,
                    iter_warmup   = 1000,
                    iter_sampling = 1000,
                    chains = 4,
                    parallel_chains = 4,
                    seed,
                    init = "cmdstan_default",
                    ...) {
  stopifnot(length(init) == 1)
  if (is.character(init))
    stopifnot(init %in% c("cmdstan_default", "pathfinder", "prior"))
  x_t <- stats::model.matrix.lm(formula_zero, data = .data,
                                na.action = stats::na.fail)
  x_r <- stats::model.matrix.lm(formula_dens, data = .data,
                                na.action = stats::na.fail)
  model_dat <- make_data_sdm(y = .data[[y_col]],
                             time = .data[[time_col]],
                             site = .data[[site_col]],
                             family = family,
                             z = x_t,
                             x = x_r,
                             ...)
  model <- instantiate::stan_package_model(
                            name = "sdm",
                            package = "drmr"
                        )
  if (init == "cmdstan_default") {
    sdm_init <- NULL
  } else if (init == "prior") {
    sdm_init <-
      prior_inits(model_dat, chains, "sdm")
  } else if (init == "pathfinder") {
    sdm_init <-
      model$pathfinder(data = model_dat,
                       seed = seed,
                       num_paths = chains,
                       save_single_paths = TRUE,
                       psis_resample = FALSE)
  } else {
    sdm_init <- init
  }
  draws <- model$sample(data = model_dat,
                        iter_warmup = iter_warmup,
                        iter_sampling = iter_sampling,
                        seed = seed,
                        chains = chains,
                        parallel_chains = parallel_chains,
                        init = sdm_init)
  output <-
    list("stanfit"  = draws,
         "data"     = model_dat,
         "formulas" = list("formula_zero" = formula_zero,
                           "formula_dens" = formula_dens),
         "cols" = list("y_col" = y_col,
                       "time_col" = time_col,
                       "site_col" = site_col))
  return(output)
}
