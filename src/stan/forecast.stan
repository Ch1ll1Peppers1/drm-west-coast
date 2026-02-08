functions {
#include utils/age_struct.stan
#include utils/age_st_forecast.stan
#include utils/lpdfs.stan
}
data {
  //--- survey data  ---
  int N;
  int n_ages; // number of ages
  int n_patches; // number of patches
  int n_time; // years for forecasts
  int n_time_train; // years for training
  array[N] int time;
  array[N] int patch;
  //--- toggles ---
  int<lower = 0, upper = 1> rho_mu;
  int<lower = 0, upper = 3> ar_re;
  int<lower = 0, upper = 3> iid_re;
  int<lower = 0, upper = 3> sp_re;
  int<lower = 0, upper = 1> movement;
  int<lower = 0, upper = 1> est_surv; // estimate mortality?
  int<lower = 0, upper = 1> cloglog; // use cloglog instead of logit for rho
  int<lower = 0, upper = 4> likelihood; // (0 = Original LN, 1 = repar LN, 2 =
                                        // Gamma, 3 = log-Logistic, 4 = truncated normal)
  //--- suitability (for rho) ----
  int<lower = 1> K_t;
  matrix[N, K_t] X_t;
  //--- fish mortality data ----
  matrix[n_ages, n_time] f;
  matrix[n_ages, n_time_train] f_past;
  array[est_surv ? 0 : 1] real m; // total mortality
  //--- movement related quantities ----
  array[movement ? n_ages : 0] int ages_movement;
  vector[n_ages] selectivity_at_age;
  //--- environmental data ----
  //--- * for mortality ----
  array[est_surv ? 1 : 0] int<lower = 1> K_m;
  matrix[est_surv ? N : 1, est_surv ? K_m[1] : 1] X_m;
  matrix[est_surv ? n_patches : 1, est_surv ? K_m[1] : 1] X_m_past;
  //--- * for recruitment ----
  int<lower = 1> K_r;
  matrix[N, K_r] X_r;
}
parameters {
  //--- "regression" coefficients for absence and recr ----
  vector[K_t] beta_t;
  vector[K_r] beta_r;
  //--- pop dyn parameters ----
  matrix[n_ages, n_patches] lambda;
  //--- rel between rho and mu ----
  array[rho_mu] real xi;
  //--- additional parameter for different lik functions ----
  array[likelihood > 0 ? 1 : 0] real phi;
  array[likelihood == 0 ? 1 : 0] real<lower = 0> sigma_obs;
  //--- movement matrix ---
  matrix[movement ? n_patches : 0, movement ? n_patches : 0] mov_mat;
  //--- reg for mortality ---
  array[est_surv] vector[est_surv ? K_m[1] : 0] beta_s;
  //--- parameters from AR process ----
  vector[ar_re > 0 ? n_time_train : 0] z_t;
  array[ar_re > 0 ? 1 : 0] real alpha;
  array[ar_re > 0 ? 1 : 0] real sigma_t;
  //--- IID RE ----
  array[iid_re > 0 ? 1 : 0] vector[n_patches] z_i;
  //--- SP RE ----
  vector[sp_re > 0 ? n_patches : 0] z_s;
}
generated quantities {
  //--- projected expected density by age ---
  array[n_ages] matrix[n_time, n_patches] lambda_proj;
  //--- projected expected density ----
  vector[n_time * n_patches] mu_proj;
  //--- projected total density ----
  vector[n_time * n_patches] y_proj;
  //--- projected absence probability ----
  vector[n_time * n_patches] rho_proj;
  //--- AR term ----
  vector[ar_re > 0 ? n_time : 0] z_tp;
  if (ar_re > 0) {
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
  //--- lambda_proj calculations ----
  {
    vector[N] log_rec;
    log_rec = X_r * beta_r;
    if (ar_re == 1) {
      for (n in 1:N)
        log_rec[n] += z_tp[time[n]];
    }
    if (iid_re == 1) {
      for (n in 1:N)
        log_rec[n] += z_i[1][patch[n]];
    }
    if (sp_re == 1) {
      for (n in 1:N)
        log_rec[n] += z_s[patch[n]];
    }
    vector[n_patches] past_m;
    matrix[n_time, n_patches] current_m;
    vector[n_time * n_patches] m_aux;
    if (!est_surv) {
      current_m = rep_matrix(- m[1], n_time, n_patches);
      past_m = rep_vector(- m[1], n_patches);
    } else {
      m_aux = X_m * beta_s[1];
      if (ar_re == 2) {
        for (n in 1:N)
          m_aux[n] += z_tp[time[n]];
      }
      if (iid_re == 2) {
        for (n in 1:N)
          m_aux[n] += z_i[1][patch[n]];
      }
      if (sp_re == 2) {
        for (n in 1:N)
          m_aux[n] += z_s[patch[n]];
      }
      current_m = to_matrix(m_aux, n_time, n_patches);
      past_m = X_m_past * beta_s[1];
    }
    // forecast_simplest is a function in the utils/theoretical_mean.stan file
    lambda_proj = forecast_simplest(n_patches,
                                    n_time,
                                    n_ages,
                                    f,
                                    current_m,
                                    to_matrix(log_rec,
                                              n_time,
                                              n_patches),
                                    lambda,
                                    f_past,
                                    past_m);
    if (movement) {
      lambda_proj =
        apply_movement(lambda_proj, mov_mat, ages_movement);
    }
    //--- mu_proj calculations ----
    matrix[n_time, n_patches] mu_aux =
      rep_matrix(0.0, n_time, n_patches);
    //--- filling mu ----
    for (tp in 1 : n_time) {
      for (p in 1 : n_patches) {
        real mu_aux2;
        mu_aux2 = dot_product(to_vector(lambda_proj[1:n_ages, tp, p]),
                              selectivity_at_age);
        if (!is_nan(mu_aux2)) {
          mu_aux[tp, p] = mu_aux2;
        }
      } // close patches
    }
    mu_proj = to_vector(mu_aux);
    if (ar_re == 3) {
      for (n in 1:N)
        mu_proj[n] *= exp(z_tp[time[n]]);
    }
    if (iid_re == 3) {
      for (n in 1:N)
        mu_proj[n] *= exp(z_i[1][patch[n]]);
    }
    if (sp_re == 3) {
      for (n in 1:N)
        mu_proj[n] *= exp(z_s[patch[n]]);
    }
  }
  //--- absence probabilities ----
  if (cloglog) {
    if (rho_mu) {
      rho_proj =
        inv_cloglog(X_t * beta_t + xi[1] .* log(mu_proj));
    } else {
      rho_proj =
        inv_cloglog(X_t * beta_t);
    }
  } else {
    if (rho_mu) {
      rho_proj =
        inv_logit(X_t * beta_t + xi[1] .* log(mu_proj));
    } else {
      rho_proj =
        inv_logit(X_t * beta_t);
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
    } else {
      array[2] real aux_tn = rep_array(0.0, 2);
      aux_tn[2] = normal_rng(mu_proj[n], phi[1]);
      y_proj[n] = (1 - bernoulli_rng(rho_proj[n])) *
        max(aux_tn);
    }
  }
}
