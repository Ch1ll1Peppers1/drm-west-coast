functions {
#include utils/lpdfs.stan
#include utils/age_struct.stan
}
data {
  //--- survey data  ---
  int N; // n_patches * n_time
  int n_ages; // number of ages
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
  int<lower = 0, upper = 3> ar_re;
  int<lower = 0, upper = 3> iid_re;
  int<lower = 0, upper = 3> sp_re;
  int<lower = 0, upper = 1> movement;
  int<lower = 0, upper = 1> est_surv; // estimate mortality?
  int<lower = 0, upper = 1> est_init; // estimate "initial cohort"
  int<lower = 0, upper = 1> minit;    // estimate "initial cohort" assuming
                                      // mortality is stable at the beginning of
                                      // the time series
  int<lower = 0, upper = 1> cloglog; // use cloglog instead of logit for rho
  int<lower = 0, upper = 4> likelihood; // (0 = Original LN, 1 = repar LN, 2 =
                                        // Gamma, 3 = log-Logistic. 4 = truncated normal)
  //--- suitability (for rho) ----
  int<lower = 1> K_t;
  matrix[N, K_t] X_t;
  //--- fish mortality data ----
  matrix[n_ages, n_time] f;
  array[est_surv ? 0 : 1] real m; // total mortality
  //--- movement related quantities ----
  matrix[movement ? n_patches: 1, movement ? n_patches : 1] adj_mat;
  array[movement ? n_ages : 0] int ages_movement;
  vector[n_ages] selectivity_at_age;
  //--- initial cohort (if not estimated) ----
  array[est_init ? 0 : n_ages - 1] real init_data;
   //--- for spatial random effects ----
  array[sp_re > 0 ? 1 : 0] int<lower = 0> N_edges;  // number of neighbor pairs
  array[sp_re > 0 ? 2 : 1,
        sp_re > 0 ? N_edges[1] : 1]
  int<lower = 1, upper = n_patches> neighbors;  // columnwise adjacent
  array[sp_re > 0 ? 1 : 0] real scaling;
  //--- environmental data ----
  //--- * for mortality ----
  array[est_surv ? 1 : 0] int<lower = 1> K_m;
  matrix[est_surv ? N : 1, est_surv ? K_m[1] : 1] X_m;
  //--- * for recruitment ----
  int<lower = 1> K_r;
  matrix[N, K_r] X_r;
  //--- priors hyperparameters ----
  real pr_sigma_obs_mu; 
  real pr_sigma_obs_sd; // formerly sigma_obs_cv
  real<lower = 0> pr_phi_a; // revise this. It is for phi in gamma, loglogistic, and
  real<lower = 0> pr_phi_b;
  real pr_lsigma_t_mu; 
  real pr_lsigma_t_sd;
  real pr_lsigma_i_mu; 
  real pr_lsigma_i_sd;
  real pr_lsigma_s_mu; 
  real pr_lsigma_s_sd;
  real pr_alpha_a; 
  real pr_alpha_b;
  real pr_zeta_a; 
  real pr_zeta_b;
  real pr_lmxi_mu;
  real pr_lmxi_sd;
  vector[K_t] pr_beta_t_mu;
  vector[K_t] pr_beta_t_sd;
  vector[est_surv ? K_m[1] : 0] pr_beta_s_mu;
  vector[est_surv ? K_m[1] : 0] pr_beta_s_sd;
  vector[K_r] pr_beta_r_mu;
  vector[K_r] pr_beta_r_sd;
}
transformed data {
  //--- Movement ----
  // `identity_mat` is a "patch X patch" identity matrix used to apply the
  // "reflexive" movement.
  matrix[movement ? n_patches : 0, movement ? n_patches : 0] identity_mat;
  if (movement)
    identity_mat = identity_matrix(n_patches);
  //--- Mortality ----
  // `fixed_m` is a "time X patch" constant matrix. All of its positions are
  // equal to the fishing mortality `m`. The purpose of this matrix is the make
  // the changes in the code when not estimating `m` minimal.
  matrix[est_surv ? 0 : n_time, est_surv ? 0 : n_patches] fixed_m;
  if (!est_surv)
    fixed_m = rep_matrix(- m[1], n_time, n_patches);
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
  vector[K_r] beta_r;
  // parameter associated with "encounter probability"
  vector[K_t] beta_t;
  // coefficients for mortality/survival (it is a log-linear model)
  array[est_surv] vector[est_surv ? K_m[1] : 0] beta_s;
  //--- * initialization parameter ----
  array[est_init ? n_ages - 1 : 0] real log_init;
  //--- * AR process parameters ----
  // conditional SD
  array[ar_re > 0 ? 1 : 0] real log_sigma_t;
  // autocorrelation
  array[ar_re > 0 ? 1 : 0] real<lower = 0, upper = 1> alpha;
  // aux latent variable
  array[ar_re > 0 ? 1 : 0] vector[n_time] w_t;
  //--- * IID RE ----
  // SD
  array[iid_re > 0 ? 1 : 0] real log_sigma_i;
  // aux latent variable
  array[iid_re > 0 ? 1 : 0] sum_to_zero_vector[n_patches] z_i;
  //--- * ICAR RE ----
  array[sp_re > 0 ? 1 : 0] sum_to_zero_vector[n_patches] w_s;
  array[sp_re > 0 ? 1 : 0] real log_sigma_s;
  //--- * Movement parameter ----
  // logit of the probability of staying in the same patch
  array[movement] real<lower = 0, upper = 1> zeta;
}
transformed parameters {
  // relationship between mu and rho
  array[rho_mu] real xi;
  if (rho_mu)
    xi = - exp(lxi);
  //--- Initialization ----
  array[est_init ? n_ages - 1 : 0] real init_par;
  if (est_init)
    init_par = log_init;
  //--- Recruitment ----
  // we are working with "log recruitment" here
  vector[N] log_rec;
  log_rec = X_r * beta_r;
  //--- Mortality ----
  vector[est_surv ? N : 0] mortality;
  if (est_surv)
    mortality = X_m * beta_s[1];
  // Expected density at specific time/patch combinations
  vector[N] mu =
    rep_vector(0.0, N);
  matrix[n_ages, n_patches] lambda =
    rep_matrix(0.0, n_ages, n_patches);
  //--- Movement ----
  // probability of staying in the current patch
  // movement matrix
  matrix[movement ? n_patches : 0, movement ? n_patches : 0] mov_mat;
  if (movement) {
    // probability of movement is evenly distributed across neighbors
    real d = (1 - zeta[1]);
    mov_mat = zeta[1] * identity_mat;
    // It is super important that the adj_mat is setup correctly. Note that,
    // this matrix is "row standardized". That is, its rows add up to 1.
    mov_mat += d * adj_mat;
  }
  //--- AR process ----
  array[ar_re > 0 ? 1 : 0] real sigma_t;
  vector[ar_re > 0 ? n_time : 0] z_t;
  if (ar_re > 0) {
    sigma_t[1] = exp(log_sigma_t[1]);
    z_t[1] = sigma_t[1] * w_t[1][1];
    for (tp in 2:n_time) {
      z_t[tp] = alpha[1] * z_t[tp - 1] +
        sigma_t[1] * w_t[1][tp];
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
  {
    //--- inputing the AR effects ----
    if (ar_re == 1) {
      for (n in 1:N)
        log_rec[n] += z_t[time[n]];
    }
    if (ar_re == 2) {
      for (n in 1:N)
        mortality[n] += z_t[time[n]];
    }
    //--- inputing IID effects ----
    if (iid_re == 1) {
      for (n in 1:N)
        log_rec[n] += z_i[1][patch[n]];
    }
    if (iid_re == 2) {
      for (n in 1:N)
        mortality[n] += z_i[1][patch[n]];
    }
     //--- inputing the AR effects ----
    if (ar_re == 1) {
      for (n in 1:N)
        log_rec[n] += z_t[time[n]];
    }
    if (ar_re == 2) {
      for (n in 1:N)
        mortality[n] += z_t[time[n]];
    }
    //--- inputing IID effects ----
    if (sp_re == 1) {
      for (n in 1:N)
        log_rec[n] += z_s[patch[n]];
    }
    if (sp_re == 2) {
      for (n in 1:N)
        mortality[n] += z_s[patch[n]];
    }
    // Expected density at specific time/patch combinations by age
    array[n_ages] matrix[n_time, n_patches] lambda_aux;
    // filling lambda according to our "simplest model"
    lambda_aux =
      simplest(n_patches, n_time, n_ages,
               f,
               est_surv ? to_matrix(mortality, n_time, n_patches) : fixed_m,
               est_init ? init_par : init_data,
               to_matrix(log_rec, n_time, n_patches),
               minit);
    //--- Movement ----
    if (movement)
      lambda_aux = apply_movement(lambda_aux, mov_mat, ages_movement);
    matrix[n_time, n_patches] mu_aux =
      rep_matrix(0.0, n_time, n_patches);
    //--- filling mu ----
    for (tp in 1 : n_time) {
      for (p in 1 : n_patches) {
        real mu_aux2;
        mu_aux2 = dot_product(to_vector(lambda_aux[1:n_ages, tp, p]),
                              selectivity_at_age);
        if (!is_nan(mu_aux2)) {
          mu_aux[tp, p] = mu_aux2;
        }
        if (tp == n_time) {
          lambda[1:n_ages, p] =
            to_vector(lambda_aux[1:n_ages, tp, p]);
        }
      } // close patches
    }
    mu = to_vector(mu_aux);
    if (ar_re == 3) {
      for (n in 1:N)
        mu[n] *= exp(z_t[time[n]]);
    }
    if (iid_re == 3) {
      for (n in 1:N)
        mu[n] *= exp(z_i[1][patch[n]]);
    }
    if (sp_re == 3) {
      for (n in 1:N)
        mu[n] *= exp(z_s[patch[n]]);
    }
  }
  //--- quantities used in the likelihood ----
  // probability of encounter at specific time/patch combinations
  vector[N] rho;
  // rho now hasa "regression like" type
  if (cloglog) {
    if (rho_mu) {
      rho =
        inv_cloglog(X_t * beta_t + xi[1] .* log(mu));
    } else {
      rho =
        inv_cloglog(X_t * beta_t);
    }
  } else {
    if (rho_mu) {
    rho =
      inv_logit(X_t * beta_t + xi[1] .* log(mu));
    } else {
    rho =
      inv_logit(X_t * beta_t);
    }
  }
}
// close transformed parameters block
model {
  //--- initialization parameters ----
  if (est_init)
    target += std_normal_lpdf(log_init);
  //--- rel between rho and mu ----
  if (rho_mu)
    target += normal_lpdf(lxi[1] | pr_lmxi_mu, pr_lmxi_sd);
  //--- AR process ----
  if (ar_re > 0) {
    target += std_normal_lpdf(w_t[1]);
    target += normal_lpdf(log_sigma_t[1] | pr_lsigma_t_mu, pr_lsigma_t_sd);
    target += beta_lpdf(alpha[1] | pr_alpha_a, pr_alpha_b); 
  }
  //--- IID RE ----
  if (iid_re > 0) {
    target += normal_lpdf(log_sigma_i[1] | pr_lsigma_i_mu, pr_lsigma_i_sd);
    target += normal_lpdf(z_i[1] | 0, sigma_i[1] * s_iid);
  }
  //--- SP RE ----
  if (sp_re > 0) {
    target += normal_lpdf(log_sigma_s[1] | pr_lsigma_s_mu, pr_lsigma_s_sd);
    target += -0.5 * dot_self(w_s[1][neighbors[1]] - w_s[1][neighbors[2]]); // ICAR prior
  }
  //--- Movement ----
  if (movement) {
    target += beta_lpdf(zeta | pr_zeta_a, pr_zeta_b);
  }
  //--- Mortality ----
  if (est_surv)
    target += normal_lpdf(beta_s[1] | pr_beta_s_mu, pr_beta_s_sd);
  //--- Recruitment ----
  target += normal_lpdf(beta_r | pr_beta_r_mu, pr_beta_r_sd);
  //--- suitability ----
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
  } else if (likelihood == 3) {
    vector[N_nz] a_ll;
    real b_ll;
    b_ll = phi[1] + 1;
    a_ll = sin(pi() / b_ll) * mu[id_nz] * b_ll / inv(pi());
    target += log1m(rho[id_nz]);
    target += loglogistic_lpdf(y[id_nz] | a_ll, b_ll);
  } else {
    target += log1m(rho[id_nz]);
    target += normal_lpdf(y | mu, phi[1]) -
      normal_lccdf(rep_vector(0.0, N) | mu, phi[1]);
  }
}
generated quantities {
  /* vector[N] log_lik; */
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
      a_ll = sin(pi() / b_ll) * mu[n] * b_ll * inv(pi());
      y_pp[n] = (1 - bernoulli_rng(rho[n])) *
        loglogistic_rng(a_ll, b_ll);
      /* if (y[n] == 0) { */
      /*   log_lik[n] = log(rho[n]); */
      /* } else { */
      /*   log_lik[n] = log1m(rho[n]) + */
      /*     loglogistic_lpdf(y[n] | a_ll, b_ll); */
      /* } */
    } else {
      array[2] real aux_tn = rep_array(0.0, 2);
      aux_tn[2] = normal_rng(mu[n], phi[1]);
      y_pp[n] = (1 - bernoulli_rng(rho[n])) *
        max(aux_tn);
      /* if (y[n] == 0) { */
      /*   log_lik[n] = log(rho[n]) + */
      /*     normal_lpdf(0.0 | mu[n], phi[1]) + */
      /*     normal_lccdf(0.0 | mu[n], phi[1]); */
      /* } else { */
      /*   log_lik[n] = log1m(rho[n]) + */
      /*     normal_lpdf(y[n] | mu[n], phi[1]) + */
      /*     normal_lccdf(0.0 | mu[n], phi[1]); */
      /* } */
    }
  }
}
