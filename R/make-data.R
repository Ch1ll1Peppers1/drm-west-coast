##' This function creates the \code{list} used as the input for the \code{stan}
##' model.
##'
##' @title Make data for DRM stan models
##' @param y a \code{numeric vector} of species' densities.
##' @param time an \code{vector} indicating the time point associated to each
##'   element of \code{y}.
##' @param site an \code{vector} indicating the sites associated to each element
##'   of \code{y}.
##' @param init_data an optional vector (of lengh n_ages - 1) to initialize the
##'   population dynamics.
##' @param f_mort an optional \code{matrix} informing the instantaneous fishing
##'   mortality rates at each age (columns) and timepoint (rows).
##' @param m a \code{numeric} value corresponding to the instantaneous natural
##'   mortality rate. The default value for this is \code{-log(.7)}, as it
##'   implies a survival rate of 0.70 between age classes.
##' @param x_t a design \code{matrix} of variables associated to the probability
##'   of absence at each site/time.
##' @param x_m a design \code{matrix} of variables associated to survival.
##' @param x_r a design \code{matrix} of variables associated to recruitment.
##' @param n_ages an \code{integer} indicating the number of ages for the
##'   underlying population dynamic model.
##' @param age_selectivity an \code{numeric vector} with \code{n_ages} elements,
##'   where each element indicates the selectivity of a respective age. All the
##'   elements of this vector must lie between 0 and 1.
##' @param ages_movement a \code{integer} indicating the age at which
##'   individuals from the focal species start moving. In this case, individuals
##'   below this age threshold remain "static". Alternatively, we can input a
##'   \code{vector} of length \code{n_ages}. This vector will have a \code{0}
##'   for age-groups that cannot move and \code{1} for those age-groups that
##'   move. For example, the following vector \code{c(0, 0, 1, 1, 0)} indicates
##'   that ages 1, 2, and 5 are static, while ages 3 and 4 are allowed to move.
##' @param ages_movement An \code{integer} or a \code{numeric vector} specifying
##'   the ages at which individuals of the focal species are assumed to move. If
##'   \code{ages_movement} is an integer, individuals younger than this age are
##'   considered static (non-moving). If \code{ages_movement} is a numeric
##'   vector of length \code{n_ages}, it indicates movement capability for each
##'   age group. A value of \code{0} indicates the corresponding age group is
##'   static, while \code{1} indicates movement is allowed. For example,
##'   \code{c(0, 0, 1, 1, 0)} specifies that age groups 1, 2, and 5 are static,
##'   while 3 and 4 are mobile.
##' @param adj_mat an adjacency \code{matrix} of dimensions \code{sites}
##'   \eqn{\times} \code{sites}. Its elements are 1 if two sites are neighbors
##'   and zero otherwise.
##' @param .toggles a \code{list} of toggles for model components. The
##'   components are: \itemize{ \item \code{rho_mu}: 1 to use explicitly relates
##'   rho to mu and 0 otherwise. \item \code{cloglog}: 1 to use the
##'   complementary log-log and 0 for the logit link function for the absence
##'   probabilities.  \item \code{movement}: 1 to allow for (adjacent) moviment;
##'   0 for static.  \item \code{est_surv}: 1 to estimate mortality and 0
##'   otherwise.  \item \code{est_init}: 1 to estimate initial values for lambda
##'   and 0 otherwise.  \item \code{minit}: 1 to use mortality to estimate
##'   initial age classes and 0 otherwise.  \item \code{ar_re}: a
##'   \code{character}. It assumes one of the following values: "none" - no AR,
##'   "rec" AR(1) for recruitment, "surv" - AR(1) for survival (only works when
##'   \code{est_surv} is on), "dens" - AR(1) for density.  \item \code{iid_re}:
##'   a \code{character}. It assumes one of the following values: "none" - no
##'   iid re, "rec" iid re for recruitment, "surv" - iir re for survival (only
##'   works when \code{est_surv} is on), "dens" - iid_re for density.  \item
##'   \code{sp_re}: a \code{character}. It assumes one of the following values:
##'   "none" - no ICAR re, "rec" ICAR re for recruitment, "surv" - ICAR re for
##'   survival (only works when \code{est_surv} is on), "dens" - ICAR_re for
##'   density.}
##' @param .priors a \code{list} of priors hyperparameters.
##' @param reorder a \code{boolean} telling whether the data needs to be
##'   reordered. The default is TRUE and means the data points will be ordered
##'   by site and time, respectively.
##' @param family a \code{character} specifying the family of the probability
##'   distribution assumed for density. The options are: \itemize{ \item
##'   \code{"gamma"} (default): gamma parametrized in terms of its mean; \item
##'   \code{"lognormal"}: log-normal parametrized in terms of its mean; \item
##'   \code{"loglogistic"}: log-logistic parametrized in terms of its mean.
##'   \item \code{"lognormal_legacy"} (default): log-normal with its usual
##'   parametrization; }
##' @param phi_hat a \code{boolean} indicating whether the prior on \code{phi}
##'   should be determined through the data.
##' @return a \code{list} to be used as the input for a \code{stan} model
##' @author lcgodoy
##' @export
make_data <- function(y,
                      time,
                      site,
                      init_data = numeric(0),
                      f_mort,
                      m = - log(.7),
                      x_t,
                      x_m,
                      x_r,
                      n_ages = 2,
                      age_selectivity,
                      ages_movement,
                      adj_mat = matrix(0, ncol = 1, nrow = 1),
                      .toggles,
                      .priors,
                      family = "gamma",
                      reorder = TRUE,
                      phi_hat = FALSE) {
  ## getting the default toggles and using user options
  stopifnot(all(!is.na(c(time))))
  stopifnot(all(!is.na(c(site))))
  zeros <- get_zeros(y)
  if (any(is.na(y)))
    y <- ifelse(is.na(y), -99, y)
  stopifnot(length(family) == 1)
  stopifnot(family %in% c("lognormal_legacy", "lognormal",
                          "gamma", "loglogistic",
                          "truncnorm"))
  likelihood <- switch(family,
                       lognormal_legacy  = 0,
                       lognormal         = 1,
                       gamma             = 2,
                       loglogistic       = 3,
                       truncnorm         = 4)
  toggles <- default_toggles() |>
    safe_modify(.toggles) |>
    c(list(likelihood = likelihood))
  ## getting the default priors and using user options
  if (phi_hat) {
    phi_hat <- get_phi_hat(y, family)
    if (missing(.priors)) {
      .priors <- list(pr_phi_a = 2, pr_phi_b = 2 / phi_hat)
    } else if (!"pr_phi_b" %in% names(.priors)) {
      if (!"pr_phi_a" %in% names(.priors)) {
        .priors <- c(.priors, list(pr_phi_a = 2, pr_phi_b = 2 / phi_hat))
      } else {
        .priors <- c(.priors, list(pr_phi_b = .priors$pr_phi_a / phi_hat))
      }
    }
  }
  priors <- default_priors() |>
    safe_modify(.priors)
  ## additional quantities that can be inferred from data
  n_patches <- length(unique(site))
  n_time <- length(unique(time))
  if (reorder) {
    my_ord <- order(site, time)
    y <- y[my_ord]
    site <- site[my_ord]
    time <- time[my_ord] - min(time) + 1
  }
  if (missing(f_mort))
    f_mort <- matrix(0, nrow = n_ages, ncol = n_time)
  if (missing(age_selectivity)) {
    selectivity_at_age <- rep(1, n_ages)
  } else {
    stopifnot(length(age_selectivity) == n_ages)
    selectivity_at_age <- age_selectivity
  }
  if (toggles$movement) {
    stopifnot(ncol(adj_mat) == nrow(adj_mat) &&
              nrow(adj_mat) == n_patches)
    if (missing(ages_movement)) {
      ages_movement <- rep(1L, n_ages)
    } else if (length(ages_movement) == 1) {
      aux_mov <- seq_len(n_ages)
      ages_movement <- ifelse(aux_mov >= ages_movement, 1, 0)
    } else {
      stopifnot(length(ages_movement) == n_ages)
    }
  } else if (missing(ages_movement)) {
    ages_movement <- integer(0)
  }
  if (!toggles$est_surv) {
    m <- array(m, dim = 1)
    x_m <- matrix(0, nrow = 1, ncol = 1)
    K_m <- integer(0)
  } else {
    stopifnot(ncol(x_m) >= 1)
    m <- numeric(0)
    if (reorder)
      x_m <- x_m[my_ord, , drop = FALSE]
    K_m <- array(NCOL(x_m), dim = 1)
    if (length(priors$pr_beta_s_mu) < K_m)
      priors$pr_beta_s_mu <- rep(0, K_m)
    if (length(priors$pr_beta_s_sd) < K_m )
      priors$pr_beta_s_sd <- rep(1, K_m)
    if (K_m == 1) {
      priors$pr_beta_s_mu <- array(priors$pr_beta_s_mu,
                                   dim = 1)
      priors$pr_beta_s_sd <- array(priors$pr_beta_s_sd,
                                   dim = 1)
    }
  }
  if (missing(x_r)) {
    x_r <- matrix(1, nrow = n_time * n_patches)
    K_r <- 1
  } else {
    K_r <- ncol(x_r)
    if (reorder)
      x_r <- x_r[my_ord, , drop = FALSE]
    if (length(priors$pr_beta_r_mu) < K_r)
      priors$pr_beta_r_mu <- rep(0, K_r)
    if (length(priors$pr_beta_r_sd) < K_r)
      priors$pr_beta_r_sd <- rep(1, K_r)
    if (K_r == 1) {
      priors$pr_beta_r_mu <- array(priors$pr_beta_r_mu,
                                   dim = 1)
      priors$pr_beta_r_sd <- array(priors$pr_beta_r_sd,
                                   dim = 1)
    }
  }
  if (missing(x_t)) {
    x_t <- matrix(1, nrow = n_time * n_patches)
    K_t <- 1
  } else {
    K_t <- ncol(x_t)
    if (reorder)
      x_t <- x_t[my_ord, , drop = FALSE]
    if (length(priors$pr_beta_t_mu) < K_t)
      priors$pr_beta_t_mu <- rep(0, K_t)
    if (length(priors$pr_beta_t_sd) < K_t)
      priors$pr_beta_t_sd <- rep(1, K_t)
    if (K_t == 1) {
      priors$pr_beta_t_mu <- array(priors$pr_beta_t_mu,
                                   dim = 1)
      priors$pr_beta_t_sd <- array(priors$pr_beta_t_sd,
                                   dim = 1)
    }
  }
  if (!toggles$est_init) {
    if (length(init_data) > 0) {
      stopifnot(length(init_data) == n_ages - 1)
    } else {
      init_data <- log(seq(from = 1, to = .01,
                           length.out = n_ages - 1))
      if (length(init_data) == 1)
        init_data <- array(init_data, dim = 1)
    }
  }
  if (toggles$sp_re > 0) {
    stopifnot(ncol(adj_mat) == nrow(adj_mat) &&
              nrow(adj_mat) == n_patches)
    aux_sp <- get_nodes(adj_mat)
    adj2 <- matrix(0, ncol = NCOL(adj_mat), nrow = NROW(adj_mat))
    adj2[adj_mat > 0] <- 1
    scaling <- array(get_scaling(adj2), dim = 1)
    N_edges <- array(as.integer(aux_sp$N_edges), dim = 1)
    neighbors <- aux_sp$neighbors
    if (!toggles$movement) {
      adj_mat <- matrix(0, ncol = 1, nrow = 1)
    }
  } else {
    scaling <- numeric(0)
    neighbors <- matrix(1, ncol = 1, nrow = 1)
    N_edges <- integer(0)
  }
  output <- list(N = n_time * n_patches,
                 n_ages = n_ages,
                 n_patches = n_patches,
                 n_time = n_time,
                 time = time,
                 patch = site,
                 init_data = init_data,
                 y = y,
                 f = f_mort,
                 m = m,
                 X_t = x_t,
                 K_t = K_t,
                 X_m = x_m,
                 K_m = K_m,
                 X_r = x_r,
                 K_r = K_r,
                 scaling = scaling,
                 neighbors = neighbors,
                 N_edges = N_edges,
                 adj_mat = adj_mat,
                 ages_movement = ages_movement,
                 selectivity_at_age = selectivity_at_age) |>
    c(zeros,
      toggles,
      priors)
  return(output)
}

##' This function creates the \code{list} used as the input for the \code{stan}
##' model.
##'
##' @title Make data for SDM stan models
##' @param y a \code{numeric vector} of species' densities.
##' @param time an \code{vector} indicating the time point associated to each
##'   element of \code{y}.
##' @param site an \code{vector} indicating the sites associated to each element
##'   of \code{y}.
##' @param z a design \code{matrix} of variables associated to the probability
##'   of absence at each site/time.
##' @param x a design \code{matrix} of variables associated to the non-zero
##'   densities.
##' @param adj_mat an adjacency \code{matrix} of dimensions \code{sites}
##'   \eqn{\times} \code{sites}. Its elements are 1 if two sites are neighbors
##'   and zero otherwise.
##' @param .toggles a \code{list} of toggles for model components. The
##'   components are: \itemize{\item \code{rho_mu}: 1 to use explicitly relate
##'   rho to mu and 0 otherwise. \item \code{cloglog}: 1 to use the
##'   complementary log-log and 0 for the logit link function for the absence
##'   probabilities.  \item \code{movement}: 1 to allow for (adjacent) moviment;
##'   0 for static.  \item \code{est_surv}: 1 to estimate survival rates and 0
##'   otherwise. \item \code{ar_re}: 1 to incorporate an AR(1) process to
##'   density and 0 otherwise. \item \code{iid_re}: 1 to incorporate a patch
##'   specific IID random effect to density and 0 otherwise. \item \code{sp_re}:
##'   1 to incorporate a patch specific ICAR random effect to density and 0
##'   otherwise.}
##' @param .priors a \code{list} of priors hyperparameters.
##' @param reorder a \code{boolean} telling whether the data needs to be
##'   reordered. The default is TRUE and means the data points will be ordered
##'   by site and time, respectively.
##' @param family a \code{character} specifying the family of the probability
##'   distribution assumed for density. The options are: \itemize{ \item
##'   \code{"gamma"} (default): gamma parametrized in terms of its mean; \item
##'   \code{"lognormal"}: log-normal parametrized in terms of its mean; \item
##'   \code{"loglogistic"}: log-logistic parametrized in terms of its mean.
##'   \item \code{"lognormal_legacy"} (default): log-normal with its usual
##'   parametrization; }
##' @param phi_hat a \code{boolean} indicating whether the prior on \code{phi}
##'   should be determined through the data.
##' @return a \code{list} to be used as the input for a \code{stan} model
##' @author lcgodoy
##' @export
make_data_sdm <- function(y,
                          time,
                          site,
                          z,
                          x,
                          adj_mat = matrix(0, ncol = 1, nrow = 1),
                          .toggles,
                          .priors,
                          family = "gamma",
                          reorder = TRUE,
                          phi_hat = FALSE) {
  ## getting the default toggles and using user options
  stopifnot(all(!is.na(c(time))))
  stopifnot(all(!is.na(c(site))))
  zeros <- get_zeros(y)
  if (any(is.na(y)))
    y <- ifelse(is.na(y), -99, y)
  stopifnot(length(family) == 1)
  stopifnot(family %in% c("lognormal_legacy", "lognormal",
                          "gamma", "loglogistic"))
  likelihood <- switch(family,
                       lognormal_legacy = 0,
                       lognormal        = 1,
                       gamma            = 2,
                       loglogistic      = 3)
  toggles <- list(rho_mu = 1,
                  ar_re = 0,
                  sp_re = 0,
                  iid_re = 0,
                  cloglog = 0) |>
    safe_modify(.toggles) |>
    c(list(likelihood = likelihood))
  stopifnot(toggles$ar_re <= 1)
  ## getting the default priors and using user options
  if (phi_hat) {
    phi_hat <- get_phi_hat(y, family)
    if (missing(.priors)) {
      .priors <- list(pr_phi_a = 2, pr_phi_b = 2 / phi_hat)
    } else if (!"pr_phi_b" %in% names(.priors)) {
      if (!"pr_phi_a" %in% names(.priors)) {
        .priors <- c(.priors, list(pr_phi_a = 2, pr_phi_b = 2 / phi_hat))
      } else {
        .priors <- c(.priors, list(pr_phi_b = .priors$pr_phi_a / phi_hat))
      }
    }
  }
  priors <- default_priors() |>
    safe_modify(.priors)
  ## additional quantities that can be inferred from data
  n_patches <- length(unique(site))
  n_time <- length(unique(time))
  if (reorder) {
    my_ord <- order(site, time)
    y <- y[my_ord]
    site <- site[my_ord]
    time <- time[my_ord]
    time <- time - min(time) + 1
  }
  if (missing(x)) {
    x <- matrix(1, nrow = n_time * n_patches)
    K_x <- 1
  } else {
    K_x <- ncol(x)
    if (reorder)
      x <- x[my_ord, , drop = FALSE]
    if (length(priors$pr_beta_r_mu) < K_x)
      priors$pr_beta_r_mu <- rep(0, K_x)
    if (length(priors$pr_beta_r_sd) < K_x)
      priors$pr_beta_r_sd <- rep(1, K_x)
    if (K_x == 1) {
      priors$pr_beta_r_mu <- array(priors$pr_beta_r_mu,
                                   dim = 1)
      priors$pr_beta_r_sd <- array(priors$pr_beta_r_sd,
                                   dim = 1)
    }
  }
  if (missing(z)) {
    z <- matrix(1, nrow = n_time * n_patches)
    K_z <- 1
  } else {
    K_z <- ncol(z)
    if (reorder)
      z <- z[my_ord, , drop = FALSE]
    if (length(priors$pr_beta_t_mu) < K_z)
      priors$pr_beta_t_mu <- rep(0, K_z)
    if (length(priors$pr_beta_t_sd) < K_z)
      priors$pr_beta_t_sd <- rep(1, K_z)
    if (K_z == 1) {
      priors$pr_beta_t_mu <- array(priors$pr_beta_t_mu,
                                   dim = 1)
      priors$pr_beta_t_sd <- array(priors$pr_beta_t_sd,
                                   dim = 1)
    }
  }
  if (toggles$sp_re > 0) {
    stopifnot(ncol(adj_mat) == nrow(adj_mat) &&
              nrow(adj_mat) == n_patches)
    aux_sp <- get_nodes(adj_mat)
    adj2 <- matrix(0, ncol = NCOL(adj_mat), nrow = NROW(adj_mat))
    adj2[adj_mat > 0] <- 1
    scaling <- array(get_scaling(adj2), dim = 1)
    N_edges <- array(as.integer(aux_sp$N_edges), dim = 1)
    neighbors <- aux_sp$neighbors
  } else {
    scaling <- numeric(0)
    neighbors <- matrix(1, ncol = 1, nrow = 1)
    N_edges <- integer(0)
  }
  output <- list(N = n_time * n_patches,
                 n_patches = n_patches,
                 n_time = n_time,
                 time = time,
                 patch = site,
                 y = y,
                 X = x,
                 K_x = K_x,
                 Z = z,
                 K_z = K_z,
                 scaling = scaling,
                 neighbors = neighbors,
                 N_edges = N_edges) |>
    c(zeros,
      toggles,
      priors)
  return(output)
}
