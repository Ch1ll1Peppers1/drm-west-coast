##' @inherit get_fitted_pars
fitted_pars_lambda <- function(data_list) {
  output <- c("log_rec")
  if (data_list$est_surv == 1)
    output <- c(output, "beta_s")
  if (data_list$movement == 1)
    output <- c(output, "mov_mat")
  if (data_list$est_init == 1)
    output <- c(output, "log_init")
  return(output)
}

##' @title Age-specific densities based on DRM.
##'
##' @description Considering a new dataset (across the same patches), computes
##'   forecasts based on the DRM passed as \code{drm}.
##'
##' @param drm A \code{list} object containing the output from the [fit_drm]
##'   function.
##' @param cores number of threads used for the forecast. If four chains were
##'   used in the \code{drm}, then four (or less) threads are recommended.
##'
##' @author lcgodoy
##'
##' @return an object of class \code{"CmdStanGQ"} containing samples for the
##'   posterior predictive distribution for forecasting.
##'
##' @export
lambda_drm <- function(drm,
                       cores = 1) {
  stopifnot(inherits(drm$stanfit, "CmdStanFit"))
  ## number time points for forecasting
  ##--- pars from model fitted ----
  pars <- fitted_pars_lambda(drm$data)
  fitted_params <-
    drm$stanfit$draws(variables = pars)
  ## load compiled lambda
  lambda_comp <-
    instantiate::stan_package_model(name = "lambda_gq",
                                    package = "drmr")
  ## computing forecast
  output <- lambda_comp$
    generate_quantities(fitted_params = fitted_params,
                        data = drm$data,
                        parallel_chains = cores)
  return(output)
}
