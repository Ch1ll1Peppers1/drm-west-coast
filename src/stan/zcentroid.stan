functions {
}
data {
  int<lower = 0> N1;
  int<lower = 0> N2;
  int<lower = 0> n_c;
  matrix[N1 + N2, n_c] coords;
  int<lower = 1> n_t;
}
transformed data {
  int N = N1 + N2;
  int n_p;
  n_p = N %/% n_t;
  array[n_c] matrix[n_t, n_p] coords_aux;
  for (k in 1:n_c) {
    coords_aux[k] = to_matrix(coords[, k], n_t, n_p);
  }
  vector[n_p] ones = rep_vector(1.0, n_p);
}
parameters {
  //--- "regression" coefficien_ts ----
  vector[N1] y_pp;
  vector[N2] y_proj;
}
transformed parameters {
  vector[N] y = append_row(y_pp, y_proj);
  matrix[n_t, n_p] ymat = to_matrix(y, n_t, n_p);
  vector[n_t] denom;
  denom = inv(ymat * ones);
}
generated quantities {
  matrix[n_t, n_c] centroid;
  matrix[n_t, n_c] inertia;
  {
    for (k in 1:n_c) {
      for (t in 1:n_t) {
        centroid[t, k] = dot_product(coords_aux[k, t, ], ymat[t, ]) *
          denom[t];
        inertia[t, k] =
          dot_product(square(coords_aux[k, t, ] - centroid[t, k]),
                      ymat[t, ]) *
          denom[t];
      }
    }
  }
}
