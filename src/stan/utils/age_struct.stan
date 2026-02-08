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
 * @param recruitment a n_time by n_patches matrix;
 * @param init_type a n_time by n_patches matrix;
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
                        matrix recruitment,
                        int minit) {
  // initializing output with zeros
  array[n_ages] matrix[n_time, n_patches] output
    = rep_array(rep_matrix(0.0, n_time, n_patches), n_ages);
  output[1] = recruitment;
  if (minit) {
    for (p in 1:n_patches) {
      for (a in 2:n_ages) {
        output[a, 1, p] = output[1, 1, p] +
          neg_mort[2, p] - f_a_t[a - 1, 2];
      }
    }
  } else {
    for (a in 1 : (n_ages - 1)) {
      output[a + 1, 1, ] = rep_row_vector(init[a], n_patches);
    }
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
 * @title Generate theoretical mean according to a Ricker model
 *
 * @description
 * 
 * @param n_patches number of patches
 * @param n_time number of years of training data
 * @param n_ages number of age classes
 * @param f_a_t fishing mortality at age "a" and time "t"
 * @param neg_mort minus natural mortality (instantaneous) rate
 * @param init a n_ages - 1 array.
 * @param rep_age a n_ages array specifying ages that contribute to
 * recruitment.
 * @param g_r growth rate.
 * recruitment.
 * 
 * @return an array of numbers by age, year and patch
 */
/* array[] matrix popricker(int n_patches, */
/*                          int n_time, */
/*                          int n_ages, */
/*                          // Mortality parameter */
/*                          matrix f_a_t, */
/*                          matrix neg_mort, */
/*                          // initialization */
/*                          array[] real init, */
/*                          array[] int rep_age, */
/*                          matrix g_r) { */
/*   // initializing output with zeros */
/*   array[n_ages] matrix[n_time, n_patches] output */
/*     = rep_array(rep_matrix(0.0, n_time, n_patches), n_ages); */
/*   for (p in 1:n_patches) { */
/*     for (i in 1:n_time) { */
/*       output[1, i, p] = recruitment[i, p]; */
/*     } */
/*   } */
/*   for (a in 1 : (n_ages - 1)) { */
/*     output[a, 1 : a, ] = rep_matrix(to_vector(init[1 : a]), n_patches); */
/*   } */
/*   /\* output[1] += init; *\/ */
/*   for (i in 2 : n_time) { */
/*     for (p in 1 : n_patches) { */
/*       for (a in 2 : n_ages) { */
/*         output[a, i, p] = output[a - 1, i - 1, p] + */
/*           neg_mort[i - 1, p] - f_a_t[a - 1, i - 1]; */
/*       } */
/*     } */
/*   } */
/*   return exp(output); */
/* } */
/**
 * @title Adding process error
 *
 * @description
 * 
 * @param lrec log recruitment for each site
 * @param z "process error"
 * 
 * @return an array of numbers by age, year and patch
 */
matrix add_pe(vector lrec, vector z) {
  array[3] int dimensions;
  int nt = num_elements(z);
  int np = num_elements(lrec) %/% nt; // %/% is integer division!
  matrix[nt, np] output = to_matrix(lrec, nt, np);
  for (p in 1:np) {
    output[, p] += z;
  }
  return output;
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
