/**
 * Gamma lpdf reparametrized.
 *
 * Reference: https://doi.org/10.1111/jtsa.12242
 * 
 * @param x non-negative random variable
 * @param mu theoretical mean of X
 * @param phi inverse scale parameter.
 * 
 * @return a log-pdf
 */
real gamma_mu_lpdf(real x, real mu, real phi) {
  return - lgamma(phi) + lmultiply(phi, phi) - lmultiply(phi, mu) +
    lmultiply(phi, x) - phi * x * inv(mu);
}

/**
 * Log-Normal lpdf reparametrized.
 *
 * Reference: https://doi.org/10.1111/jtsa.12242
 * 
 * @param x non-negative random variable
 * @param mu theoretical mean of X
 * @param phi weird parameter parameter.
 * 
 * @return a log-pdf
 */
real ln_mu_lpdf(real x, real mu, real phi) {
  real f_mu_phi;
  f_mu_phi = log1p(phi * inv_square(mu));
  real output = 0;
  output += log2() + log(pi()) + log(f_mu_phi) + log(x) +
    square(log(x) + log2() - 0.5 * f_mu_phi) * inv(f_mu_phi);
  return - 0.5 * output;
}

/**
 * Inverse Gaussian lpdf reparametrized.
 *
 * Reference: https://doi.org/10.1111/jtsa.12242
 * 
 * @param x non-negative random variable
 * @param mu theoretical mean of X
 * @param phi weird parameter parameter.
 * 
 * @return a log-pdf
 */
real igaus_mu_lpdf(real x, real mu, real phi) {
  real f_mu_phi;
  real output = 0;
  output += log2() + log(pi()) + 3 * log(x) - log(phi) +
    log(phi) * square(x - mu) * inv(mu * mu * x);
  return - 0.5 * output;
}

real igaus_mu_rng(real mu, real phi) {
  real nu;
  real output;
  real z;
  nu = std_normal_rng();
  z = uniform_rng(0, 1);
  output = square(nu);
  output = mu + square(mu) * output * inv(2 * phi) -
    mu * inv(2 * phi) *
    sqrt(4 * mu * phi * output + square(mu * output));
  if (z <= mu * inv(mu + output)) {
    return output;
  } else {
    return square(mu) * inv(output);
  }
}
