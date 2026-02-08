functions {
#include utils/lpdfs.stan
}
data {
  //--- survey data  ---
  int N;
  int n_patches; // number of patches
  int n_time; // years for forecasts
  int n_time_train; // years for training
  array[N] int time;
  array[N] int<lower = 1, upper = n_patches> patch;
  //--- toggles ---
  int<lower = 0, upper = 1> rho_mu;
  int<lower = 0, upper = 1> ar_re;
  int<lower = 0, upper = 1> iid_re;
  int<lower = 0, upper = 1> sp_re;
  int<lower = 0, upper = 1> cloglog; // use cloglog instead of logit for rho
  int<lower = 0, upper = 3> likelihood; // (0 = Original LN, 1 = repar LN, 2 =
                                        // Gamma, 3 = log-Logistic)
  //--- suitability (for rho) ----
  int<lower = 1> K_z;
  matrix[N, K_z] Z;
  //--- environmental data ----
  int<lower = 1> K_x;
  matrix[N, K_x] X;
}
parameters {
  //--- "regression" coefficients ----
  vector[K_z] beta_t;
  vector[K_x] beta_r;
  array[rho_mu] real xi;
  //--- additional parameter for different lik functions ----
  array[likelihood > 0 ? 1 : 0] real phi;
  array[likelihood == 0 ? 1 : 0] real<lower = 0> sigma_obs;
  //--- parameters from AR process ----
  vector[ar_re ? n_time_train : 0] z_t;
  array[ar_re] real alpha;
  array[ar_re] real sigma_t;
  //--- IID RE ----
  array[iid_re ? 1 : 0] vector[n_patches] z_i;
  //--- SP RE ----
  vector[sp_re ? n_patches : 0] z_s;
}
generated quantities {
  //--- projected expected density ----
  vector[n_time * n_patches] mu_proj;
  //--- projected total density ----
  vector[n_time * n_patches] y_proj;
  //--- projected absence probability ----
  vector[n_time * n_patches] rho_proj;
  //--- AR term ----
  vector[ar_re ? n_time : 0] z_tp;
  if (ar_re) {
    {
      vector[n_time] w_t;
      w_t[1] = std_normal_rng();
      z_tp[1] = alpha[1] * z_t[n_time_train] +
        sigma_t[1] * w_t[1];
      for (tp in 2:n_time) {
        w_t[tp] = std_normal_rng();
        z_tp[tp] = alpha[1] * z_tp[tp - 1] +
          sigma_t[1] * w_t[tp];
      }
    }
  }
  //--- mu_proj calculations ----
  {
    vector[N] lmu;
    lmu = X * beta_r;
    if (ar_re) {
      for (n in 1:N)
        lmu[n] += z_tp[time[n]];
    }
    if (sp_re) {
      for (n in 1:N)
        lmu[n] += z_s[patch[n]];
    }
    if (iid_re) {
      for (n in 1:N)
        lmu[n] += z_i[1][patch[n]];
    }
    mu_proj = exp(lmu);
    //--- absence probabilities ----
    if (cloglog) {
      if (rho_mu) {
      rho_proj =
        inv_cloglog(Z * beta_t + xi[1] .* lmu);
      } else {
      rho_proj =
        inv_cloglog(Z * beta_t);
      }
    } else {
      if (rho_mu) {
        rho_proj =
          inv_logit(Z * beta_t + xi[1] .* lmu);
      } else {
        rho_proj =
          inv_logit(Z * beta_t);
      }
    }
  }
  //--- y_proj calculations ----
  for (n in 1:N) {
    if (likelihood == 0) {
      real loc_par;
      loc_par = log(mu_proj[n]) + square(sigma_obs[1]) / 2;
      y_proj[n] = (1 - bernoulli_rng(rho_proj[n])) *
        lognormal_rng(loc_par, sigma_obs[1]);
    } else if (likelihood == 1) {
      real mu_ln;
      real sigma_ln;
      sigma_ln = sqrt(log1p(phi[1] * inv_square(mu_proj[n])));
      mu_ln = log(square(mu_proj[n]) * inv_sqrt(square(mu_proj[n]) + phi[1]));
      y_proj[n] = (1 - bernoulli_rng(rho_proj[n])) *
        lognormal_rng(mu_ln, sigma_ln);
    } else if (likelihood == 2) {
      real gamma_beta;
      gamma_beta = phi[1] / mu_proj[n];
      y_proj[n] = (1 - bernoulli_rng(rho_proj[n])) *
        gamma_rng(phi[1], gamma_beta);
    } else if (likelihood == 3) {
      real a_ll;
      real b_ll;
      b_ll = phi[1] + 1;
      a_ll = sin(pi() / b_ll) * mu_proj[n] * inv(pi() * b_ll);
      y_proj[n] = (1 - bernoulli_rng(rho_proj[n])) *
        loglogistic_rng(a_ll, b_ll);
    }
  }
}
