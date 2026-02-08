array[] matrix forecast_simplest(int n_patches,
                                 int n_time,
                                 int n_ages,
                                 // Mortality parameter
                                 matrix f_a_t,
                                 matrix neg_mort,
                                 // initialization (currently with recruitment)
                                 matrix init,
                                 // from past
                                 matrix lambda_past,
                                 matrix f_past,
                                 vector neg_mort_past) {
  // initializing output with zeros
  array[n_ages] matrix[n_time, n_patches] output
    = rep_array(init, n_ages);
  int past_last_time;
  past_last_time = cols(f_past[1]);
  /* output[1] += init; */
  for (i in 1 : n_time) {
    for (p in 1 : n_patches) {
      for (a in 2 : n_ages) {
        if (i == 1) {
          output[a, i, p] = log(lambda_past[a - 1, p])
            + neg_mort_past[p] - f_past[a - 1, past_last_time];
        } else {
          output[a, i, p] = output[a - 1, i - 1, p]
            + neg_mort[i - 1, p] - f_a_t[a - 1, i - 1];
        }
      }
    }
  }
  return exp(output);
}
