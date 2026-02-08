##' @title Regression coefficient for non-centered variable
##' @description Consider a linear predictor having linear and square terms
##'   associated with a variable \eqn{x}. Assume this variable was centered
##'   before being included in the linear predictor. This functions recovers the
##'   regression coefficient associated to the linear term as if the variable
##'   was not centered.
##' @param beta1 A \code{numeric} regression coefficient associated to the
##'   linear term.
##' @param beta2 A \code{numeric} regression coefficient associated to the
##'   quadratic term.
##' @param offset a \code{numeric} representing the "center" of \eqn{x}.
##' @return a \code{numeric} representing the regression coefficient of the
##'   linear term for the model where \eqn{x} is not centered.
##' @author Lucas Godoy
##' @export
fix_linbeta <- function(beta1, beta2, offset) {
  beta1 - 2 * offset * beta2
}

##' @title Value of a covariate that maximizes the response variable in a
##'   quadratic model.
##' @description Consider a linear predictor having linear and square terms
##'   associated with a variable \eqn{x}. Assume this variable was centered
##'   before being included in the linear predictor. This functions returns the
##'   value of \eqn{x} (on its original scale) such that the linear predictor is
##'   maximized (or minimized).
##' @param beta1 A \code{numeric} regression coefficient associated to the
##'   linear term.
##' @param beta2 A \code{numeric} regression coefficient associated to the
##'   quadratic term.
##' @param offset a \code{numeric} representing the "center" of \eqn{x}.
##' @return a \code{numeric} representing the uncentered \eqn{x} that maximizes
##'   (or minimizes) the linear predictor.
##' @author Lucas Godoy
##' @export
max_quad_x <- function(beta1, beta2, offset = 0) {
  stopifnot(NROW(beta1) == NROW(beta2))
  stopifnot(NROW(offset) == 1)
  out <- fix_linbeta(beta1, beta2, offset)
  - .5  * out / beta2
}

##' Generates an adjacency matrix for "movement"
##' @title Generates an adjacency matrix
##' @param x an \code{sf} object representing the patches.
##' @return an adjacency \code{matrix}
##' @author lcgodoy
##' @export
gen_adj <- function(x) {
  if (!inherits(x, "sfc")) {
    message("Assuming patches are 1d and ordered in such a way that the first and last patches have only one neighbor.")
    aux <-
      rbind(c(0, 0), c(1, 0),
            c(1, 1), c(0, 0)) |>
      list() |>
      sf::st_polygon() |>
      sf::st_sfc()
    out <- sf::st_make_grid(aux,
                            n = c(1, length(x)),
                            what = "polygons")
  } else
    out <- x
  spdep::poly2nb(out) |>
    spdep::nb2mat(style = "B") |>
    as.matrix()
}

##' Safely modifies a named \code{list}.
##'
##' This function returns an error if any of the names found in the
##' \code{replacements} object are not in the \code{list} to be modified.
##' @title Modifying a named list
##' @param original a named \code{list} with "original" parameters.
##' @param replacements a named \code{list} containing elements to be modified
##'   in the \code{original} \code{list}
##' @return an updated \code{original list}.
##' @author lcgodoy
safe_modify <- function(original, replacements) {
  if (!missing(replacements)) {
    stopifnot(all(names(replacements) %in% names(original)))
    if ("ar_re" %in% names(replacements)) {
      replacements[["ar_re"]] <-
        fix_re(replacements[["ar_re"]])
    }
    if ("iid_re" %in% names(replacements)) {
      replacements[["iid_re"]] <-
        fix_re(replacements[["iid_re"]])
    }
    if ("sp_re" %in% names(replacements)) {
      replacements[["sp_re"]] <-
        fix_re(replacements[["sp_re"]])
    }
    out <- utils::modifyList(original, replacements)
  } else {
    out <- original
  }
  return(out)
}

##' @title Check if x is between lb and ub
##'
##' @param x A numeric vector.
##' @param lb A numeric vector of lower bounds.
##' @param ub A numeric vector of upper bounds.
##'
##' @return No return value. This function stops execution if any of the
##'  following conditions are met:
##'  * lb is greater than or equal to ub.
##'  * The lengths of lb and ub are not equal.
##'  * The lengths of lb, ub, and x are not equal.
##'
check_between <- function(x, lb, ub) {
  stopifnot(all(lb <= ub))
  if (length(lb) > 1 | length(ub) > 1) {
    stopifnot(length(lb) == length(ub) &
              length(lb) == length(x))
  }
}

##' @inherit between
between_scalar <- function(x, lb, ub) {
  x >= lb & x <= ub
}

##' @title Check if elements in x are between corresponding elements in lb and
##'   ub
##'
##' @inheritParams check_between
##'
##' @return A logical vector of the same length as x, indicating whether each
##'   element of x is between the corresponding elements of lb and ub.
##'
##' @examples
##' between(1:5, 1, 5)
##' between(1:5, 2, 4)
##'
##' @export
between <- function(x, lb, ub) {
  check_between(x, lb, ub)
  if (length(lb) == 1) {
    sapply(x, between_scalar, lb, ub)
  } else {
    mapply(between_scalar, x, lb, ub)
  }
}

##' @title Calculate the interval score
##'
##' @description This function calculates the interval score for a given set of
##'   observations, lower and upper bounds, and alpha parameter.
##'
##' @details The interval score is a proper scoring rule that measures the
##'   accuracy of interval predictions. It takes into account both the coverage
##'   and the width of the prediction interval. A lower score indicates a better
##'   prediction.
##'
##' @param y A numeric vector of observations.
##' @param l A numeric vector of lower bounds for the prediction intervals.
##' @param u A numeric vector of upper bounds for the prediction intervals.
##' @param alpha A numeric value specifying the penalty parameter for interval
##'   width.
##'
##' @return A numeric vector of interval scores.
##'
##' @export
int_score <- function(y, l, u, alpha) {
  stopifnot(alpha > 0 & alpha < 1)
  check_between(y, l, u)
  alpha <- 2 / alpha
  ind_l <- as.numeric(y < l)
  ind_u <- as.numeric(y > u)
  (u - l) + alpha * ((l - y) * ind_l + (y - u) * ind_u)
}

##' @title Relationships with covariates
##' @details the \code{marg_rec} function works to obtain relationships
##'   regarding recruitment, while \code{marg_surv} and \code{marg_abs} evaluate
##'   relationships with survival and absence probability, respectively.
##' @param drm the output of a [fit_drm()] call.
##' @param newdata a \code{data.frame} with the values of the environmental
##'   variables at which we wish to estimate the recruitment, survival, or
##'   absence probabilities.
##' @return a \code{data.frame} with the samples the recruitment (or survival,
##'   or absence probabilities) posterior distribution.
##' @name marg
##' @export
##' @author lcgodoy
marg_rec <- function(drm, newdata) {
  stopifnot(inherits(drm$stanfit, "CmdStanFit"))
  my_formula <- drm$formulas$formula_rec
  new_x <- stats::model.matrix(my_formula, newdata)
  est_samples <-
    drm$stanfit$draws(variables = c("beta_r"),
                      format = "draws_matrix")
  n_sim <- NROW(est_samples)
  output <- vector(mode = "list", length = NROW(newdata))
  est_samples <-
    tcrossprod(est_samples, new_x)
  est_samples <- exp(c(est_samples))
  rownames(newdata) <- NULL
  for (i in seq_along(output)) {
    output[[i]] <-
      cbind.data.frame(newdata[i, , drop = FALSE], iter = seq_len(n_sim)) |>
      suppressWarnings()
  }
  output <- dplyr::bind_rows(output)
  output <- dplyr::mutate(output, recruitment = est_samples)
  return(output)
}

##' @rdname marg
##' @export
marg_pabs <- function(drm, newdata) {
  stopifnot(inherits(drm$stanfit, "CmdStanFit"))
  my_formula <- drm$formulas$formula_zero
  new_x <- stats::model.matrix(my_formula, newdata)
  est_samples <-
    drm$stanfit$draws(variables = c("beta_t"),
                      format = "draws_matrix")
  n_sim <- NROW(est_samples)
  output <- vector(mode = "list", length = NROW(newdata))
  est_samples <-
    tcrossprod(est_samples, new_x)
  est_samples <- stats::plogis(c(est_samples))
  rownames(newdata) <- NULL
  for (i in seq_along(output)) {
    output[[i]] <-
      cbind.data.frame(newdata[i, , drop = FALSE], iter = seq_len(n_sim)) |>
      suppressWarnings()
  }
  output <- dplyr::bind_rows(output)
  output <- dplyr::mutate(output, prob_abs = est_samples)
  return(output)
}

##' @rdname marg
##' @export
marg_surv <- function(drm, newdata) {
  stopifnot(inherits(drm$stanfit, "CmdStanFit"))
  stopifnot(!is.null(drm$data$K_m))
  my_formula <- drm$formulas$formula_surv
  new_x <- stats::model.matrix(my_formula, newdata)
  est_samples <-
    drm$stanfit$draws(variables = c("beta_s"),
                      format = "draws_matrix")
  n_sim <- NROW(est_samples)
  output <- vector(mode = "list", length = NROW(newdata))
  est_samples <-
    tcrossprod(est_samples, new_x)
  est_samples <- exp(c(est_samples))
  rownames(newdata) <- NULL
  for (i in seq_along(output)) {
    output[[i]] <-
      cbind.data.frame(newdata[i, , drop = FALSE], iter = seq_len(n_sim)) |>
      suppressWarnings()
  }
  output <- dplyr::bind_rows(output)
  output <- dplyr::mutate(output, survival = est_samples)
  return(output)
}

##' @title Random effects verbose to code
##' @param x a \code{character}
##' @return a \code{integer}
##' @author lcgogoy
fix_re <- function(x) {
  if (!x %in% c(0:3)) {
    switch(x,
           none = 0,
           rec  = 1,
           surv = 2,
           dens = 3)
  } else x
}

##' @title Get nodes for ICAR spatial random effects
##' @param adj adjacency \code{matrix}
##' @return a \code{list}.
##' @author lcgodoy
get_nodes <- function(adj) {
  ladj <- apply(adj > 0, MARGIN = 1,
                which)
  nodes <-
    Map(\(row, col) {
      matrix(c(rep(as.integer(row),
                   length(col)),
               col), ncol = 2)
    }, names(ladj), ladj)
  nodes <- do.call(rbind, nodes)
  return(list("N_edges" = NROW(nodes),
              "neighbors" = t(nodes)))
}

##' @title Generalized Inverse
##'
##' @details this function is taken from the package \code{MASS}
##' 
##' @param X A matrix we wish to invert.
##' @param tol A relative tolerance to detect zero singular values.
##' @return The generalized inverse of \code{X}
ginv <- function (X, tol = sqrt(.Machine$double.eps)) {
    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    Xsvd <- svd(X)
    if (is.complex(X)) 
        Xsvd$u <- Conj(Xsvd$u)
    Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
    if (all(Positive)) 
        Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
    else if (!any(Positive)) 
        array(0, dim(X)[2L:1L])
    else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
        t(Xsvd$u[, Positive, drop = FALSE]))
}

##' @title Scaling factor for ICAR
##'
##' @description Using results from Rue and Held 2005 and Morris et al. 2019.
##' 
##' @param adj adjacency \code{matrix}
##' @return a \code{scalar}
##' @author lcgodoy
get_scaling <- function(adj) {
  Q <- matrix(0, nrow = nrow(adj), ncol = ncol(adj))
  diag(Q) <- apply(adj, 1, sum)
  Q <- Q - adj
  jitter <- max(diag(Q)) * sqrt(.Machine$double.eps)
  Q <- Q +
    diag(jitter,
         ncol = NCOL(Q), nrow = NROW(Q))  
  Q_inv <- tryCatch(
      solve(Q),
      error = function(e) ginv(Q)
  )
  A <- matrix(1, nrow = 1, ncol = NCOL(Q))
  QA <- tcrossprod(Q_inv, A)
  Q_inv_const <- Q_inv - tcrossprod(QA, QA) / as.numeric(A %*% QA)
  vars <- diag(Q_inv_const)
  exp(mean(log(vars[vars > 0])))
}

##' @title Estimate phi
##' @inheritParams make_data
##' @return a \code{numeric} scalar representing an estimate for phi
##' @author lcgodoy
get_phi_hat <- function(y, family) {
  if (family == "gamma") {
    ng0 <- sum(y > 0)
    xbar <- mean(y[y > 0])
    xbar2 <- xbar * xbar
    s2 <- stats::var(y[y > 0]) * (ng0 - 1) / ng0
    return(xbar2 * s2)
  } else if (family == "lognormal") {
    lxbar <- mean(log(y[y > 0]))
    ng0 <- sum(y > 0)
    ls2 <- stats::var(log(y[y > 0])) * (ng0 - 1) / ng0
    muhat <- exp(lxbar + 0.5 * ls2)
    return(muhat * muhat * (exp(ls2) - 1))
  } else {
    return(1)
  }
}

##' @title Zeros and nonzeros
##' @inheritParams make_data
##' @return a \code{list} containing necessary input for stan to identify zeros
##'   and nonzeros in the response variable.
##' @author lcgodoy
get_zeros <- function(y) {
  stopifnot(NCOL(y) == 1)
  stopifnot(all(y[!is.na(y)] >= 0))
  id_z  <- which(y == 0)
  id_nz <- which(y > 0)
  N_z  <- length(id_z)
  N_nz <- length(id_nz)
  return(list(id_z = id_z,
              id_nz = id_nz,
              N_z = N_z,
              N_nz = N_nz))
}
