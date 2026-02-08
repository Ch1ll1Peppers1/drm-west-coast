##' @title Density of a truncated Student's t distribution
##' @description Calculates the density of a truncated Student's t distribution.
##' @param x A \code{numeric vector} at which to evaluate the density.
##' @param mu A \code{numeric} scalar representing the mean of the underlying t
##'   distribution. Defaults to 0.
##' @param sd A \code{numeric} scalar representing the standard deviation of the
##'   underlying t distribution. Defaults to 1.
##' @param df A \code{numeric} scalar representing the degrees of freedom of the
##'   underlying t distribution. Defaults to 3.
##' @param range A \code{numeric} vector of length 2 specifying the lower and
##'   upper truncation bounds. Defaults to \code{c(0, Inf)}, indicating
##'   truncation from below at 0.
##' @param log \code{Logical}; if \code{TRUE}, the natural logarithm of the
##'   density is returned.  Defaults to \code{FALSE}.
##' @return A numeric vector of the same length as \code{x}, containing the
##'   (log) density values of the truncated t distribution.
##' @seealso [stats::dt()], [stats::pt()]
##' @author lcgodoy
dtt <- function(x, mu = 0, sd = 1,
                df = 3, range = c(0, Inf),
                log = FALSE) {
  ll <- stats::pt(q = min(range - mu) / sd, df = df)
  hh <- stats::pt(q = max(range - mu) / sd, df = df)
  ## 1/sd is the Jacobian of the transformation.
  out <- stats::dt((x - mu) / sd, df = df, log = TRUE) -
    log(hh - ll) +
    log(sd)
  if (!log)
    out <- exp(out)
  return(out)
}

##' @title Random number generation from a truncated Student's t distribution
##' @description Generates random numbers from a truncated Student's t
##'   distribution.
##' @details For details on the method used, see:
##'   [https://stats.stackexchange.com/questions/567944/how-can-i-sample-from-a-shifted-and-scaled-student-t-distribution-with-a-specifi](https://stats.stackexchange.com/questions/567944/how-can-i-sample-from-a-shifted-and-scaled-student-t-distribution-with-a-specifi)
##' @param n An \code{integer} specifying the number of random numbers to
##'   generate.
##' @param mu A \code{numeric} scalar representing the mean of the underlying t
##'   distribution. Defaults to 0.
##' @param sd A \code{numeric} scalar representing the standard deviation of the
##'   underlying t distribution. Defaults to 1.
##' @param df A \code{numeric} scalar representing the degrees of freedom of the
##'   underlying t distribution. Defaults to 3.
##' @param range A \code{numeric vector} of length 2 specifying the lower and
##'   upper truncation bounds. Defaults to `c(0, Inf)`, indicating truncation
##'   from below at 0.
##' @return A \code{numeric vector} of length \code{n} containing random numbers
##'   drawn from the specified truncated t distribution.
##' @seealso [stats::rt()], [stats::qt()]
##' @author lcgodoy
rtt <- function(n, mu = 0, sd = 1,
                df = 3, range = c(0, Inf)) {
  ll <- stats::pt(q = min(range - mu) / sd, df = df)
  hh <- stats::pt(q = max(range - mu) / sd, df = df)
  u  <- stats::runif(n, min = ll, max = hh)
  mu + stats::qt(p = u, df = df) * sd
}

##' @title Density of a truncated Normal distribution
##' @description Calculates the density of a truncated Normal distribution.
##' @param x A \code{numeric vector} at which to evaluate the density.
##' @param mean A \code{numeric} scalar representing the mean of the underlying
##'   normal distribution. Defaults to 0.
##' @param sd A \code{numeric} scalar representing the standard deviation of the
##'   underlying normal distribution. Defaults to 1.
##' @param range A \code{numeric} vector of length 2 specifying the lower and
##'   upper truncation bounds. Defaults to `c(0, Inf)`, indicating truncation
##'   from below at 0.
##' @param log \code{Logical}; if \code{TRUE}, the natural logarithm of the
##'   density is returned. Defaults to \code{FALSE}.
##' @return A numeric vector of the same length as \code{x}, containing the
##'   (log) density values of the truncated normal distribution.
##' @seealso [stats::dnorm()], [stats::pnorm()]
##' @author lcgodoy
dtn <- function(x, mean = 0, sd = 1, range = c(0, Inf),
                log = FALSE) {
  ll <- stats::pnorm(q = min(range), mean = mean, sd = sd)
  hh <- stats::pnorm(q = max(range), mean = mean, sd = sd)
  out <- stats::dnorm(x, mean = mean, sd = sd,
                      log = TRUE) - log(hh - ll)
  if (!log)
    out <- exp(out)
  return(out)
}

##' @title Random number generation from a truncated Normal distribution
##' @description Generates random numbers from a truncated Normal distribution.
##' @param n An \code{integer} specifying the number of random samples to
##'   generate.
##' @param mean A \code{numeric} scalar representing the mean of the underlying
##'   normal distribution. Defaults to 0.
##' @param sd A \code{numeric} scalar representing the standard deviation of the
##'   underlying normal distribution. Defaults to 1.
##' @param range A \code{numeric vector} of length 2 specifying the lower and
##'   upper truncation bounds. Defaults to \code{c(0, Inf)}, indicating
##'   truncation from below at 0.
##' @return A \code{numeric vector} of length \code{n} containing random numbers
##'   drawn from the specified truncated normal distribution.
##' @seealso [stats::rnorm()], [stats::qnorm()]
##' @author lcgodoy
rtn <- function(n, mean = 0, sd = 1, range = c(0, Inf)) {
  ll <- stats::pnorm(q = min(range), mean = mean, sd = sd)
  hh <- stats::pnorm(q = max(range), mean = mean, sd = sd)
  u  <- stats::runif(n, min = ll, max = hh)
  stats::qnorm(p = u, mean = mean, sd = sd)
}

##' @title Validate input data for [prior_sample()]
##' @description Checks if the provided data list is valid for a given model.
##'   This is an internal function not intended for direct use.
##' @param dat A \code{list} containing the data, typically generated by
##'   [make_data()] or [make_data_sdm()].
##' @param model A \code{character} string specifying the model type. Must be
##'   either "drm" or "sdm". Defaults to "drm".
##' @return This function does not return a value. It stops execution with an
##'   error message if the data is not valid for the specified model.
##' @seealso [make_data()], [make_data_sdm()]
##' @keywords internal
##' @export
##' @author lcgodoy
check_pars <- function(dat, model) {
  if (model == "drm") {
    names_check <-
      make_data(y = 1, time = 1, site = 1)
  } else {
    names_check <-
      make_data_sdm(y = 1, time = 1, site = 1)
  }
  stopifnot(all(names(dat) %in% names(names_check)))
}

##' @title Generate samples from the prior distribution of model parameters
##' @description Generates samples from the prior distributions of the model
##'   parameters. This is primarily used for prior predictive checks or to
##'   generate initial values for MCMC.
##' @param dat A \code{list} containing the data and prior parameters, typically
##'   generated by [make_data()] or [make_data_sdm()].
##' @param model A \code{character} string specifying the model type. Must be
##'   either "drm" or "sdm". Defaults to "drm".
##' @return A \code{list} containing samples drawn from the prior distributions
##'   of the model parameters.
##' @seealso [prior_inits()]
##' @export
##' @author lcgodoy
prior_sample <- function(dat, model = "drm") {
  check_pars(dat, model)
  time_re <- ifelse(dat$ar_re != "none", 1, 0)
  iid_re <- ifelse(dat$iid_re != "none", 1, 0)
  sp_re <- ifelse(dat$sp_re != "none", 1, 0)
  out <-
    list(beta_t = stats::rnorm(ifelse(model == "drm",
                                      dat$K_t, dat$K_z),
                               mean = dat$pr_beta_t_mu,
                               sd = dat$pr_beta_t_sd),
         beta_r = stats::rnorm(ifelse(model == "drm",
                                      dat$K_r, dat$K_x),
                               mean = dat$pr_beta_r_mu,
                               sd = dat$pr_beta_r_sd))
  if (length(out$beta_t) == 1)
    out$beta_t <- array(out$beta_t, dim = 1)
  if (length(out$beta_r) == 1)
    out$beta_r <- array(out$beta_r, dim = 1)
  if (dat$rho_mu == 1) {
    out <- c(out, list("lxi" =
                         array(stats::rnorm(1, mean = dat$pr_lmxi_mu,
                                            sd = dat$pr_lmxi_sd))))
  }
  if (dat$likelihood == 0) {
    out <-
      c(out,
        list(sigma_obs = array(rtn(1,
                                   mean = dat$pr_sigma_obs_mu,
                                   sd = dat$pr_sigma_obs_sd,
                                   range = c(0, Inf)),
                               dim = 1)))
  } else {
    out <-
      c(out,
        list(phi = array(stats::rgamma(1,
                                       shape = dat$pr_phi_a,
                                       rate = dat$pr_phi_b),
                         dim = 1)))
  }
  if (time_re) {
    out <-
      c(out,
        list(log_sigma_t = array(stats::rnorm(1,
                                              dat$pr_lsigma_t_mu,
                                              dat$pr_lsigma_t_sd),
                                 dim = 1),
             alpha   = array(stats::rbeta(1,
                                          dat$pr_alpha_a,
                                          dat$pr_alpha_b),
                             dim = 1),
             w_t     = stats::rnorm(dat$n_time)))
  }
  if (iid_re) {
    out <-
      c(out,
        list(log_sigma_i = array(stats::rnorm(1,
                                              dat$pr_lsigma_i_mu,
                                              dat$pr_lsigma_i_sd),
                                 dim = 1),
             z_i     = stats::rnorm(dat$n_time)))
  }
  if (sp_re) {
    out <-
      c(out,
        list(log_sigma_s = array(stats::rnorm(1,
                                              dat$pr_lsigma_i_mu,
                                              dat$pr_lsigma_i_sd),
                                 dim = 1),
             w_s     = stats::rnorm(dat$n_time)))
  }
  if (model == "drm") {
    if (dat$movement) {
      out <-
        c(out,
          list(zeta = array(stats::rbeta(1,
                                         dat$pr_zeta_a,
                                         dat$pr_zeta_b),
                            dim = 1)))
    }
    if (dat$est_surv) {
      out <- c(out,
               list(beta_s = stats::rnorm(dat$K_m,
                                          mean = dat$pr_beta_s_mu,
                                          sd = dat$pr_beta_s_sd)))
      if (length(out$beta_s) == 1)
        out$beta_s <- array(out$beta_s, dim = 1)
    }
  }
  return(out)
}

##' @title Generate initial values for MCMC from the prior
##' @description Generates initial values for Markov Chain Monte Carlo (MCMC) by
##'   sampling from the prior distributions of the model parameters. This helps
##'   in starting the MCMC chains from different points in the parameter space.
##' @param dat A list containing the data and prior parameters, typically
##'   generated by [make_data()] or [make_data_sdm()].
##' @param chains An integer specifying the number of MCMC chains to initialize.
##'   Defaults to 4.
##' @param model A \code{character} string specifying the model type. Must be
##'   either "drm" or "sdm". Defaults to "drm".
##' @return A \code{list} of lists, where each inner list contains initial
##'   values for one MCMC chain. The structure of each inner list is the same as
##'   the output of \code{prior_sample()}.
##' @seealso [prior_sample()]
##' @export
##' @author lcgodoy
prior_inits <- function(dat, chains, model = "drm") {
  check_pars(dat, model)
  out <-
    replicate(chains, prior_sample(dat, model),
              simplify = FALSE)
  return(out)
}
