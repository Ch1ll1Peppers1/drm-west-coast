functions {
  /**
   * @title Generate theoretical mean according to the simplest model possible
   *
   * @description
   * 
   * @param n_patches number of patches
   * @param n_time number of years of training data
   * @param n_ages number of age classes
   * @param f_a_t fishing mortality at age "a" and time "t"
   * @param neg_mort minus natural mortality (instantaneous) rate
   * @param init a n_ages - 1 array.
   * 
   * @return an array of numbers by age, year and patch
   */
  array[] matrix simplest(int n_patches,
                          int n_time,
                          int n_ages,
                          // Mortality parameter
                          matrix f_a_t,
                          matrix neg_mort,
                          // initialization
                          array[] real init,
                          matrix recruitment) {
    // initializing output with zeros
    array[n_ages] matrix[n_time, n_patches] output
      = rep_array(rep_matrix(0.0, n_time, n_patches), n_ages);
    for (p in 1:n_patches) {
      for (i in 1:n_time) {
        output[1, i, p] = recruitment[i, p];
      }
    }
    for (a in 1 : (n_ages - 1)) {
      output[a, 1 : a, ] = rep_matrix(to_vector(init[1 : a]), n_patches);
    }
    /* output[1] += init; */
    for (i in 2 : n_time) {
      for (p in 1 : n_patches) {
        for (a in 2 : n_ages) {
          output[a, i, p] = output[a - 1, i - 1, p] +
            neg_mort[i - 1, p] - f_a_t[a - 1, i - 1];
        }
      }
    }
    return exp(output);
  }
  /**
   * @title Applying movement
   *
   * @description
   * 
   * @param lambda array of number of individuals per age, time, and patch
   * @param M movement matrix
   * @param mov_age ages at which movement starts (this can be generalized)
   * 
   * @return an array of numbers by age, year and patch
   */
  array[] matrix apply_movement(array[] matrix lambda, matrix M,
                                array[] int mov_age) {
    array[3] int dimensions;
    dimensions = dims(lambda);
    array[dimensions[1]] matrix[dimensions[2], dimensions[3]] output;
    output = lambda;
    for (a in 1:dimensions[1]) {
      if (mov_age[a]) {
        for (time in 1:dimensions[2]) {
          output[a, time, 1:dimensions[3]] =
            lambda[a, time, 1:dimensions[3]] * M';
        }
      }
    }
    return output;
  }
}
data {
  //--- survey data  ---
  int N; // n_patches * n_time
  int n_ages; // number of ages
  int n_patches; // number of patches
  int n_time; // years for training
  //--- toggles ---
  int<lower = 0, upper = 1> movement;
  int<lower = 0, upper = 1> est_surv; // estimate mortality?
  int<lower = 0, upper = 1> est_init; // estimate "initial cohort"
  //--- fish mortality data ----
  matrix[n_ages, n_time] f;
  array[est_surv ? 0 : 1] real m; // total mortality
  //--- movement related quantities ----
  array[movement ? n_ages : 0] int ages_movement;
  vector[n_ages] selectivity_at_age;
  //--- initial cohort (if not estimated) ----
  array[est_init ? 0 : n_ages - 1] real init_data;
  //--- environmental data ----
  //--- * for mortality ----
  array[est_surv ? 1 : 0] int<lower = 1> K_m;
  matrix[est_surv ? N : 1, est_surv ? K_m[1] : 1] X_m;
}
transformed data {
  matrix[est_surv ? 0 : n_time, est_surv ? 0 : n_patches] fixed_m;
  if (!est_surv)
    fixed_m = rep_matrix(- m[1], n_time, n_patches);
}
parameters {
  // coefficients for recruitment (it is a log-linear model)
  vector[N] log_rec;
  // coefficients for mortality/survival (it is a log-linear model)
  vector[est_surv ? K_m[1] : 0] beta_s;
  //--- * movement ----
  matrix[movement ? n_patches : 0, movement ? n_patches : 0] mov_mat;
  //--- * initialization parameter ----
  array[est_init ? n_ages - 1 : 0] real log_init;
}
transformed parameters {
}
generated quantities {
  // Expected density at specific time/patch combinations
  array[n_ages] matrix[n_time, n_patches] lambda;
  {
    //--- Initialization ----
    array[est_init ? n_ages - 1 : 0] real init_par;
    if (est_init)
      init_par = exp(log_init);
    //--- Mortality ----
    vector[est_surv ? N : 0] mortality;
    if (est_surv)
      mortality = X_m * beta_s;

    // filling lambda according to our "simplest model"
    lambda =
      simplest(n_patches, n_time, n_ages,
               f,
               est_surv ? to_matrix(mortality, n_time, n_patches) : fixed_m,
               est_init ? init_par : init_data,
               to_matrix(log_rec, n_time, n_patches));
  }
  //--- Movement ----
  if (movement)
    lambda = apply_movement(lambda, mov_mat, ages_movement);
}
