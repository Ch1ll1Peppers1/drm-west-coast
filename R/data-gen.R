##' @title Simulate AR(1)
##' @param pars a named \code{list} with two elements: \code{alpha}
##'   (representing the temporal autocorrelation parameter) and \code{tau}
##'   representing the conditional standard deviation of the AR(1) process.
##' @param n_time number of time points
##' @return a \code{vector} of length \code{n_time} representing a realization
##'   of a zero-mean AR(1) process.
##' @author lcgodoy
sim_ar <- function(pars,
                   n_time) {
  out <- vector(mode = "numeric", length = n_time)
  out[1] <- stats::rnorm(1, sd = pars[["tau"]])
  for (i in 2:n_time)
    out[i] <- pars[["alpha"]] * out[i - 1] +
      stats::rnorm(1, sd = pars[["tau"]])
  return(out)
}

##' @title Simulate log-recruitment
##'
##' @param n_patches number of patches
##' @param n_time number of timepoints
##' @param x_rec matrix of environmental factors affecting recruitment
##' @param ar_time a \code{boolean} indicating whether an AR(1) term should be
##'   included to the log-recruitment.
##' @param pars a named \code{list} of parameters used to simulate
##'   log-recruitment. It must contain a vector named \code{"beta_r"} with
##'   length equal to the number of columnts in \code{x_rec}. In addition, if
##'   \code{ar_time = TRUE}, the list must also contain a named \code{vector}
##'   called "ar". This named \code{vector} must contain an element called
##'   \code{alpha} (the autocorrelation parameter) and another called \code{tau}
##'   (the conditional SD).
##' @return a \code{matrix} with \code{n_patches} columns and \code{n_time} rows
##'   representing the log-recruitment at each patch/site and time.
##' @author lcgodoy
sim_log_rec <- function(n_patches,
                        n_time,
                        x_rec,
                        pars,
                        ar_time = TRUE) {
  betas <- pars[["beta_r"]]
  stopifnot(NCOL(x_rec) == length(betas))
  out <- as.numeric(x_rec %*% betas) |>
    matrix(ncol = n_patches, nrow = n_time)
  if (ar_time) {
    pars_ar <- c("alpha" = pars[["alpha"]],
                 "tau" = pars[["tau"]])
    ar_rec <- sim_ar(pars_ar,
                     n_time)
    out <- apply(out, 2, \(x) x + ar_rec)
  }
  return(out)
}

##' @title Generate the "survival" terms
##'
##' @param n_patches number of patches
##' @param n_time number of timepoints
##' @param x_sv matrix of environmental factors affecting survival
##' @param pars a named \code{list} of parameters used to simulate
##'   log-recruitment. It must contain a vector named \code{"beta_s"} with
##'   length equal to the number of columnts in \code{x_sv}.
##' @return a \code{matrix} with \code{n_patches} columns and \code{n_time} rows
##'   representing the log-survival at each patch/site and time.
##' @author lcgodoy
make_surv <- function(n_patches,
                      n_time,
                      x_sv,
                      pars) {
  betas <- pars[["beta_s"]]
  stopifnot(NCOL(x_sv) == length(betas))
  out <- as.numeric(x_sv %*% betas) |>
    matrix(ncol = n_patches, nrow = n_time)
  return(out)
}

##' @title Simple movement to population dynamics
##'
##' @description Apply "simple" movement to population dynamics.
##'
##' @param lambda array of number of individuals per age, time, and patch
##' @param M movement matrix
##' @param mov_age ages at which movement starts (this can be generalized)
##' @return an array of numbers by age, year and patch
##' @author lcgodoy
apply_movement <- function(lambda, M, mov_age) {
  dimensions <- dim(lambda)
  output <- lambda
  for (a in 1:dimensions[1]) {
    if (mov_age[a]) {
      for (time in 1:dimensions[2]) {
        output[a, time, 1:dimensions[3]] <-
          tcrossprod(lambda[a, time, 1:dimensions[3]], M)
      }
    }
  }
  return(output)
}

##' @title Simulate population dynamics
##'
##' @description Given a set of parameters, simulate population dynamics.
##'
##' @inheritParams sim_log_rec
##' @inheritParams make_surv
##' @param f_a_t fishing mortality
##' @param m a \code{numeric} value indicating the natural mortality
##'   instantaneous rate.
##' @param n_ages a \code{integer} indicating the number of (assumed)
##'   age-classes.
##' @param pars a named \code{list} of model parameters.
##' @param init initialization vector of length \code{n_ages - 1}
##' @param init_type type of initialization (integer between 0 and 5)
##' @param movement a \code{boolean} indicating whether movement should be
##'   applied or not. If TRUE, than \code{pars} must have an element called
##'   \code{zeta} indicating the probability of staying at a given patch between
##'   two timepoints.
##' @param adj_mat a \code{n_patches} by \code{n_patches} row-standardized
##'   adjacency \code{matrix}.
##' @param mov_age a \code{vector} of ages at which movement starts.
##' @return an array of expected densities per age-group, patch, and timepoint.
##' @author lcgodoy
pop_dyn <- function(n_patches,
                    n_time, n_ages,
                    x_rec, ar_time,
                    f_a_t, x_sv = NULL,
                    m = 0.25,
                    pars,
                    init, init_type,
                    movement = FALSE,
                    adj_mat = NULL,
                    mov_age = NULL) {
  output <- array(0, dim = c(n_ages, n_time, n_patches))
  recruitment <- sim_log_rec(n_patches, n_time,
                             x_rec, pars, ar_time)
  ## initializing recruitment elements
  output[1, , ] <- recruitment
  ## Initialization based on init_type
  if (init_type == 1) {
    ## initializes every element at the recruitment
    for (a in 1:n_ages) {
      output[a, , ] <- recruitment
    }
  } else if (init_type %in% c(2, 4)) {
    for (a in 2:n_ages) {
      ## makes more sense
      output[a, 1, ] <- rep(log(init[a - 1]), n_patches)
    }
  } else if (init_type %in% c(3, 5)) {
    for (a in 2:n_ages) {
      output[a, 1, ] <- log(init[a - 1]) + recruitment[1, ]
    }
  }
  ## given the initialization, does the order of the for loops matter?
  if (is.null(x_sv)) {
    x_sv <- matrix(1, ncol = 1, nrow = n_patches * n_time)
    surv_pars <- list("beta_s" = m)
  } else {
    surv_pars <- pars
  }
  neg_mort <- make_surv(n_patches, n_time,
                        x_sv, surv_pars)
  for (p in 1:n_patches) {
    for (i in 2:n_time) {
      output[2:n_ages, i, p] <- output[1:(n_ages - 1), i - 1, p] +
        neg_mort[i - 1, p] - f_a_t[1:(n_ages - 1), i - 1]
    }
  }
  if (movement) {
    stopifnot(!is.null(adj_mat))
    stopifnot(!is.null(mov_age))
    zeta <- pars[["zeta"]]
    mov_mat <- zeta * diag(1, ncol = n_patches, nrow = n_patches) +
      (1 - zeta) * adj_mat
    output <- exp(output)
    output <- apply_movement(output, mov_mat, mov_age)
  } else {
    output <- exp(output)
  }
  return(output)
}

##' @title Transform parameters to a meaningful and interpretable scale.
##' @param pars \code{list} of parameters
##' @return \code{list} of parameters
##' @author lcgodoy
pars_transform <- function(pars) {
  names_pars <- names(pars)
  names_pars <- gsub("0$", "", names_pars)
  names(pars) <- names_pars
  if ("logit_zeta" %in% names_pars)
    pars <- c(pars, "zeta" = stats::plogis(pars[["logit_zeta"]]))
  if ("log_tau" %in% names_pars)
    pars <- c(pars, "tau" = exp(pars[["log_tau"]]))
  if ("shift_alpha" %in% names_pars) {
    pars <- c(pars, "alpha" = 2 * pars[["shift_alpha"]] - 1)
  }
  return(pars)
}

##' @title Turn an array of density per age, time, and patch/site into a
##'   \code{data.frame}
##' @param lbd a 3-dimensional \code{array}
##' @return return a \code{data.frame}.
##' @author lcgodoy
lambda2df <- function(lbd) {
  dim_lambdas <- dim(lbd)
  dimnames(lbd) <-
    list("age"  = seq_len(dim_lambdas[1]),
         "time" = seq_len(dim_lambdas[2]),
         "site" = seq_len(dim_lambdas[3]))
  out <-
    array2DF(lbd,
             responseName = "density")
  out <- out |>
    transform(age = as.integer(out[["age"]]),
              time = as.integer(out[["time"]]),
              site = as.integer(out[["site"]]))
  return(out)
}

##' @title Simulate response variable
##' @description Given the expected value \code{x} and a list of parameters
##'   (\code{pars}), this function simulates one realization of the probability
##'   density function which is also specified by the list \code{pars}.
##' @param x an expected value for a simulated value.
##' @param pars a \code{list} of parameters.
##' @return a \code{numeric} value representing a realization of the probability
##'   distribution with mean \code{x} and parameters \code{pars}.
##' @author lcgodoy
sim_dens <- function(x, pars) {
  if (pars$likelihood == 0) {
    stats::rlnorm(1,
                  meanlog = log(x) + 0.5 * pars$sigma_obs * pars$sigma_obs,
                  sdlog = pars$sigma_obs)
  } else if (pars$likelihood == 1) {
    sqx <- x * x
    sigma_ln <- sqrt(log1p(pars$phi / sqx))
    mu_ln <- log(sqx / sqrt(sqx + pars$phi))
    stats::rlnorm(1, meanlog = mu_ln, sdlog = sigma_ln)
  } else if (pars$likelihood == 2) {
    gamma_beta <- pars$phi / x
    stats::rgamma(1, pars$phi, gamma_beta)
  } else stop("Likelihood not supported for prior predictive check yet.")
}

##' @title Generate a random sample from a model's predictive distribution given
##'   a set of parameters
##' @description Generates samples from the predictive distribution of a model
##'   given a set of parameters. This is primarily used for prior predictive
##'   checks and simulation studies.
##'
##' @details See
##'   \href{https://forum.posit.co/t/how-to-solve-no-visible-binding-for-global-variable-note/28887/3}{this
##'   link} for the [rlang::.data] import.
##' @param dat A \code{list} containing the data and prior parameters, typically
##'   generated by [make_data()] or [make_data_sdm()].
##' @param model A \code{character} string specifying the model type. Must be
##'   either "drm" or "sdm". Defaults to "drm".
##' @param selectivity a numeric \code{vector} with the same length as the
##'   number of age-groups.
##' @param pars a named \code{list} of model parameters.
##' @param ... parameters passed on to \code{pop_dyn}.
##' @return A \code{list} containing samples drawn from the prior distributions
##'   of the model parameters.
##' @importFrom rlang .data
##' @seealso [prior_inits()]
##' @author lcgodoy
model_sim <- function(dat, model,
                      selectivity,
                      pars,
                      ...) {
  lambda <- pop_dyn(n_patches = dat$n_patches,
                    n_time = dat$n_time,
                    n_ages = dat$n_ages,
                    f_a_t = dat$f,
                    pars = pars,
                    x_rec = dat$X_r,
                    ...) |>
      lambda2df()
  mu_y <- lambda
  if (!is.null(selectivity)) {
    mu_y <- dplyr::left_join(mu_y,
                             dplyr::tibble(age = seq_along(selectivity),
                                           selectivity = selectivity),
                             by = "age")
    mu_y <- mu_y |>
      dplyr::group_by(.data$site, .data$time) |>
      dplyr::summarise(avg_dens =
                         as.numeric(crossprod(.data$density, selectivity)),
                       .groups = "drop") |>
      dplyr::ungroup()
  } else {
    mu_y <- mu_y |>
      dplyr::group_by(.data$site, .data$time) |>
      dplyr::summarise(avg_dens = sum(.data$density),
                       .groups = "drop") |>
      dplyr::ungroup()
  }
  rho <- stats::plogis(as.numeric(dat[["X_t"]] %*% pars$beta_t))
  pars <- c(pars, "likelihood" = dat[["likelihood"]])
  mu_y <- mu_y |>
    dplyr::mutate(abs_prob = rho)
  mu_y[["dens"]] <- sapply(mu_y$avg_dens, sim_dens, pars)
  mu_y[["absence"]] <- sapply(mu_y$abs_prob,
                              \(x)
                              stats::rbinom(n = 1,
                                            size = 1,
                                            prob = x))
  mu_y <- mu_y |>
    dplyr::mutate(y = (1 - .data$absence) * .data$dens)
  return(list("lambda" = lambda,
              "mu_and_y" = mu_y))
}

##' @title Generate samples from the prior predictive distribution of model
##'   parameters
##' @description Generates samples from the prior distributions of the model
##'   parameters. This is primarily used for prior predictive checks or to
##'   generate initial values for MCMC.
##' @inheritParams model_sim
##' @return A \code{list} containing samples drawn from the prior distributions
##'   of the model parameters.
##' @seealso [prior_inits()]
##' @author lcgodoy
pp_sim <- function(dat, model = "drm",
                   selectivity = NULL,
                   ...) {
  pars <- prior_inits(dat, chains = 1,
                      model)[[1]] |>
    pars_transform()
  out <- model_sim(dat, model = "drm",
                   selectivity = NULL,
                   pars,
                   ...)
  out <- c(out,
           "pars" = list(pars),
           "hyper_pars" = list(dat[grepl("^pr_", names(dat))]))
  return(out)
}

## ##' @title Generate samples from the prior predictive distribution of model
## ##'   parameters
## ##' @description Generates samples from the prior distributions of the model
## ##'   parameters. This is primarily used for prior predictive checks or to
## ##'   generate initial values for MCMC.
## ##' @param n a \code{integer} representing the number of samples form the
## ##'   posterior predictive distribution.
## ##' @inheritParams model_sim
## ##' @param ... parameters passed to [pop_dyn()].
## ##' @return A \code{list} containing samples drawn from the prior distributions
## ##'   of the model parameters.
## ##' @seealso [prior_inits()] [pps_aux()]
## ##' @author lcgodoy
## prior_predictive_sim <- function(n, dat, model,
##                                  selectivity,
##                                  ...) {
##   out <-
##     replicate(n, pps_aux(dat, model,
##                          selectivity, ...),
##               simplify = FALSE)
##   return(out)
## }
