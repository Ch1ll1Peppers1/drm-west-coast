functions {
#include utils/lpdfs.stan
}
data {
  //--- survey data  ---
  int N; // n_patches * n_time
  int n_patches; // number of patches
  int n_time; // years for training
  array[N] int<lower = 1, upper = n_time> time;
  array[N] int<lower = 1, upper = n_patches> patch;
  vector[N] y;
  //--- for vectorizing zero-inflation ----
  int N_nz;
  int N_z;
  array[N_nz] int id_nz;
  array[N_z] int id_z;
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
  //--- for spatial random effects ----
  array[sp_re > 0 ? 1 : 0] int<lower = 0> N_edges;  // number of neighbor pairs
  array[sp_re > 0 ? 2 : 1,
        sp_re > 0 ? N_edges[1] : 1]
  int<lower = 1, upper = n_patches> neighbors;  // columnwise adjacent
  array[sp_re > 0 ? 1 : 0] real scaling;
  //--- environmental data ----
  //--- * for counts ----
  int<lower = 1> K_x;
  matrix[N, K_x] X;
  //--- priors hyperparameters ----
  real pr_sigma_obs_mu; 
  real pr_sigma_obs_sd; // formerly sigma_obs_cv
  real pr_phi_a; // revise this. It is for phi in gamma, loglogistic, and
                 // inverse-gaussian
  real pr_phi_b;
  // * now AR SD parameters have pcpriors
  real pr_lsigma_t_mu; 
  real pr_lsigma_t_sd;
  real pr_lsigma_i_mu; 
  real pr_lsigma_i_sd;
  real pr_lsigma_s_mu; 
  real pr_lsigma_s_sd;
  real pr_alpha_a; 
  real pr_alpha_b;
  real pr_lmxi_mu;
  real pr_lmxi_sd;
  vector[K_z] pr_beta_t_mu;
  vector[K_z] pr_beta_t_sd;
  vector[K_x] pr_beta_r_mu;
  vector[K_x] pr_beta_r_sd;
}
transformed data {
  //--- scaling factors ----
  real s_iid = sqrt(n_patches / (n_patches - 1.0));
}
parameters {
  // relationship between mu and rho
  array[rho_mu] real lxi;
  // parameter associated to the likelihood 
  array[likelihood == 0 ? 1 : 0] real<lower = 0> sigma_obs;
  array[likelihood > 0 ? 1 : 0] real<lower = 0> phi;
  // coefficients for recruitment (it is a log-linear model)
  vector[K_x] beta_r;
  // parameter associated with "encounter probability"
  vector[K_z] beta_t;
  //--- * AR process parameters ----
  // conditional SD
  array[ar_re] real log_sigma_t;
  // autocorrelation
  array[ar_re] real<lower = 0, upper = 1> alpha;
  // aux latent variable
  vector[ar_re ? n_time : 0] w_t;
  //--- * IID RE ----
  // SD
  array[iid_re] real log_sigma_i;
  // aux latent variable
  array[iid_re] sum_to_zero_vector[n_patches] z_i;
  //--- * ICAR RE ----
  array[sp_re] sum_to_zero_vector[n_patches] w_s;
  array[sp_re] real log_sigma_s;
}
transformed parameters {
  // relationship between mu and rho
  array[rho_mu] real xi;
  if (rho_mu)
    xi = - exp(lxi);
  //--- AR process ----
  array[ar_re] real sigma_t;
  vector[ar_re ? n_time : 0] z_t;
  {
    if (ar_re) {
      sigma_t[1] = exp(log_sigma_t[1]);
      z_t[1] = sigma_t[1] * w_t[1];
      for (tp in 2:n_time) {
        z_t[tp] += alpha[1] * z_t[tp - 1] +
          sigma_t[1] * w_t[tp];
      }
    }
  }
  //--- IID RE ----
  array[iid_re > 0 ? 1 : 0] real sigma_i;
  if (iid_re > 0) {
    sigma_i[1] = exp(log_sigma_i[1]);
  }
  //--- ICAR RE ----
  array[sp_re > 0 ? 1 : 0] real sigma_s;
  vector[sp_re > 0 ? n_patches : 0] z_s;
  if (sp_re > 0) {
    sigma_s[1] = exp(log_sigma_s[1]);
    z_s = sigma_s[1] * inv_sqrt(scaling[1]) * w_s[1];
  }
  //--- quantities used in the likelihood ----
  // probability of encounter at specific time/patch combinations
  vector[N] rho;
  // Expected density at specific time/patch combinations
  vector[N] mu =
    rep_vector(0.0, N);
  // rho now hasa "regression like" type
  {
    vector[N] lmu;
    lmu = X * beta_r;
    if (ar_re) {
      for (n in 1:N)
        lmu[n] += z_t[time[n]];
    }
    if (sp_re) {
      for (n in 1:N)
        lmu[n] += z_s[patch[n]];
    }
    if (iid_re) {
      for (n in 1:N)
        lmu[n] += z_i[1][patch[n]];
    }
    if (cloglog) {
      if (rho_mu) {
      rho =
        inv_cloglog(Z * beta_t + xi[1] .* lmu);
      } else {
      rho =
        inv_cloglog(Z * beta_t);
      }
    } else {
      if (rho_mu) {
      rho =
        inv_logit(Z * beta_t + xi[1] .* lmu);
      } else {
        rho =
          inv_logit(Z * beta_t);
      }
    }
    mu = exp(lmu);
  }
}
// close transformed parameters block
model {
  //--- rel between rho and mu ----
  if (rho_mu)
    target += normal_lpdf(lxi[1] | pr_lmxi_mu, pr_lmxi_sd);
  //--- AR process ----
  if (ar_re) {
    target += std_normal_lpdf(w_t);
    target += normal_lpdf(log_sigma_t[1] | pr_lsigma_t_mu, pr_lsigma_t_sd);
    target += beta_lpdf(alpha[1] | pr_alpha_a, pr_alpha_b); 
  }
  //--- IID RE ----
  if (iid_re) {
    target += normal_lpdf(log_sigma_i[1] | pr_lsigma_i_mu, pr_lsigma_i_sd);
    target += normal_lpdf(z_i[1] | 0, sigma_i[1] * s_iid);
  }
  //--- SP RE ----
  if (sp_re) {
    target += normal_lpdf(log_sigma_s[1] | pr_lsigma_s_mu, pr_lsigma_s_sd);
    target += -0.5 * dot_self(w_s[1][neighbors[1]] - w_s[1][neighbors[2]]); // ICAR prior
  }
  //--- Counts ----
  target += normal_lpdf(beta_r | pr_beta_r_mu, pr_beta_r_sd);
  //--- Suitability ----
  target += normal_lpdf(beta_t | pr_beta_t_mu, pr_beta_t_sd);
  //--- Likelihood ----
  if (likelihood == 0) {
    target += normal_lpdf(sigma_obs[1] | pr_sigma_obs_mu, pr_sigma_obs_sd) -
      1.0 * normal_lccdf(0 | pr_sigma_obs_mu, pr_sigma_obs_sd);
  } else {
    // change these parameters (PC prior for exponential?)
    target += gamma_lpdf(phi[1] | pr_phi_a, pr_phi_b);
  }
  // only evaluate density if there are length comps to evaluate
  target += sum(log(rho[id_z]));
  if (likelihood == 0) {
    vector[N_nz] loc_par;
    loc_par = log(mu[id_nz]) + square(sigma_obs[1]) / 2;
    target += log1m(rho[id_nz]);
    target += lognormal_lpdf(y[id_nz] | loc_par, sigma_obs[1]);
  } else if (likelihood == 1) {
    vector[N_nz] mu_ln;
    vector[N_nz] sigma_ln;
    sigma_ln = sqrt(log1p(phi[1] * inv_square(mu[id_nz])));
    mu_ln = log(square(mu[id_nz]) .*
                inv_sqrt(square(mu[id_nz]) + phi[1]));
    target += log1m(rho[id_nz]);
    target += lognormal_lpdf(y[id_nz] | mu_ln, sigma_ln);
  } else if (likelihood == 2) {
    vector[N_nz] b_g;
    b_g = phi[1] / mu[id_nz];
    target += log1m(rho[id_nz]);
    target += gamma_lpdf(y[id_nz] | phi[1], b_g);
  } else {
    vector[N_nz] a_ll;
    real b_ll;
    b_ll = phi[1] + 1;
    a_ll = sin(pi() / b_ll) * mu[id_nz] * b_ll * inv(pi());
    target += log1m(rho[id_nz]);
    target += loglogistic_lpdf(y[id_nz] | a_ll, b_ll);
  }
}
generated quantities {
  vector[N] log_lik;
  vector[N] y_pp;
  for (n in 1:N) {
    if (likelihood == 0) {
      real loc_par;
      loc_par = log(mu[n]) + square(sigma_obs[1]) / 2;
      y_pp[n] = (1 - bernoulli_rng(rho[n])) *
        lognormal_rng(loc_par, sigma_obs[1]);
      /* if (y[n] == 0) { */
      /*   // only evaluate density if there are length comps to evaluate */
      /*   log_lik[n] = log(rho[n]); */
      /* } else { */
      /*   log_lik[n] = log1m(rho[n]) + */
      /*     lognormal_lpdf(y[n] | loc_par, sigma_obs[1]); */
      /* } */
    } else if (likelihood == 1) {
      real mu_ln;
      real sigma_ln;
      sigma_ln = sqrt(log1p(phi[1] * inv_square(mu[n])));
      mu_ln = log(square(mu[n]) * inv_sqrt(square(mu[n]) + phi[1]));
      y_pp[n] = (1 - bernoulli_rng(rho[n])) *
        lognormal_rng(mu_ln, sigma_ln);
      /* if (y[n] == 0) { */
      /*   log_lik[n] = log(rho[n]); */
      /* } else { */
      /*   log_lik[n] = log1m(rho[n]) + */
      /*     ln_mu_lpdf(y[n] | mu[n], phi[1]); */
      /* } */
    } else if (likelihood == 2) {
      real gamma_beta;
      gamma_beta = phi[1] / mu[n];
      y_pp[n] = (1 - bernoulli_rng(rho[n])) *
        gamma_rng(phi[1], gamma_beta);
      /* if (y[n] == 0) { */
      /*   log_lik[n] = log(rho[n]); */
      /* } else { */
      /*   log_lik[n] = log1m(rho[n]) + */
      /*     gamma_lpdf(y[n] | phi[1], gamma_beta); */
      /* } */
    } else if (likelihood == 3) {
      real a_ll;
      real b_ll;
      b_ll = phi[1] + 1;
      a_ll = sin(pi() / b_ll) * mu[n]  * b_ll * inv(pi());
      y_pp[n] = (1 - bernoulli_rng(rho[n])) *
        loglogistic_rng(a_ll, b_ll);
      /* if (y[n] == 0) { */
      /*   log_lik[n] = log(rho[n]); */
      /* } else { */
      /*   log_lik[n] = log1m(rho[n]) + */
      /*     loglogistic_lpdf(y[n] | a_ll, b_ll); */
      /* } */
    }
  }
}
