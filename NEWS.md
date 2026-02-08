# drmr 0.2.2

* Ensuring there are no `NA`s in the covariates, `site`, or `time`.

* Added support for missingness on `y`.

* Functions for vectorzing the zero-inflated densities were moved to `R` (as
  opposed to `Stan`). Therefore, the indexes of the "zeros" in the response
  variables become an input from `R` to `Stan`.

* Spatial and iid random effects also included in the SDM functions.

* Typos in the "Parameters, priors, and toggles" vignette were fixed.

* `instantiate` version must be at least `0.2.3.9002`, so we can use `#include`
  for functions.

# drmr 0.2.1

* A new toggle called `rho_mu` introduced. Its default value is `1` and stands
  for explicitly relating the probability of observing a 0 (`rho`) to the latent
  density (`mu`). See [Yee 2014](https://doi.org/10.1016/j.csda.2013.01.012) and
  references therein.

* A new vignette with a comprehensive list of the model parameters and their
  priors is available.

* A new vignette detailing the initialization procedures is also included.

# drmr 0.2.0

* `init` now admits real numbers too.

* `init_data`: default now is to initialize the age-classes for all patches
  ranging from `.9` to `.01`.

* Allowing for choosing which process is correlated in time;

  * The variable `raw` became `w_t`;
  * `tau` became `sigma_t`
  * The flag `time_ar` became `ar_re` (stands for AR random effect). Now,
    `ar_re` can assume three values: 
    - "none", the default indicating no AR random effects.
    - "rec" (AR for recruitment)
    - "surv" (AR for survival)
    - "dens" (AR for density)

* Unstructured random effects.
  
  * The following variables are added to the model: `z_i` patch specific random
    effect and `sigma_i` the SD of the iid random effect.
  * Similarly to `ar_re`, the unstructured random effects can be enabled through
    the flat `iid_re`, which also admits the following entries:
    - "none" (default) indicating no IID random effects.
    - "rec" (IID for recruitment)
    - "surv" (IID for survival)
    - "dens" (iID for density)

* ICAR random effects
  
  * The following variables are added to the model: `w_s`, `z_s` patch specific
    random effect and `sigma_s` the approx marginal SD of the spatial random
    effect.
  * Similarly to the two structures mentioned above, the flag `sp_re` admits the
    following values: "none" (default), "rec" (recruitment), "surv" (survival),
    and "dens" (density).

* Functions for data simulation were temporarily removed.

* Fixing `init_data`

* Fixing bugs regarding the popdyn initialization.

# drmr 0.1.3

* Modified `examples.qmd` to prevent errors due to pathfinder failing.

* Updated github actions.

* Same problematic behaviour that was affecting `make_data` had to be dealed
  with prior initialization and predictions.
  
* Options for "data informed" phi priors through the floag `phi_hat`
  
  * When `family = "gamma"` and a prior for `phi` is not provided, the default
    prior for this parameter is a Gamma with `shape = 2` and `rate = ybar *
    s2`. This prior has its theoretical mean at the method of moments estimator
    of `phi`.

  * When `family = "lognormal"` and a prior for `phi` is not provided, the
    default prior for this parameter is a Gamma having its theoretical mean at
    the MLE of `phi`.

* fixed typo when defining log-logistic reparametrization

# drmr 0.1.2.9

* fixed weird behaviour with `make_data`

# drmr 0.1.2

* `lambda_drm` function added to recover the age-specific densities form a
  `fit_drm` call.
  
* fixed a bug on the `forecast.stan` script.

# drmr 0.1.1

* Vignettes updated so `R CMD check` "works".

* The `lambda`s are no longer returned by the fit and predict
  functions. Returning those values imply a significant decrease in the
  computational performance, especially as increasing the sample size.

* The AR term in the model was "corrected".

* Some bugs with prior initialization for the sdm were fixed.

# drmr 0.1.0

* Exporting `fix_linbeta`, `max_quad_x`, and `int_score` functions

* Fixing `check_between` function (not exported; This functions is just a
  helper)
   
* Changed prior on `zeta` and allowing for user to input the
  hyperparameters. Before, we had a standard normal prior on the logit of
  `zeta`. Now, we place a beta prior on `zeta`.

* Included support for different types of population dynamics initialization

* Fixed how pdf and random number generation from location-scale Student's t
  distribution

* `rlang` became a dependency.

* Included functions for simulating data from the DRM model and prior predictive
  checks.
   
* Trying to avoid overflow by making calculations on the log-scale whenever it's
  possible.

* Constraining `alpha` to $(0, 1)$.

* Prior on `phi` now is Gamma.

* `fit_drm` and `fit_sdm` returns changed. Now, the element `draws` is called
  `stanfit`. In addition, there is an additional element to the returned list
  called `formulas`. The `formulas` elements aims at making the `predict_*`
  functions less error prone.

* The QR parametrization toggles were completely removed.

* The `coef_*` parameters were converted to `beta_*`. That is, `coef_r` now is
  `beta_r`; while `coef_t` now is `beta_t`, and so on.

* The `pr_logsd_r_*` inputs converted to `pr_ltau_*`.

* Functions to plot effect of covariates on recruitment, survival, or absence
  probability were included.

* `est_mort` becomes `est_surv` (as it makes more sense with the text).

* `get-started` and `examples` vignette updated.

# drmr 0.0.24

* Initial values for population dynamics have been fixed.

* Vignettes to quarto

* New `between` function based on `data.table::between`.

* `int_score` function to calculate the interval score was also included. The
  interval score helps to assess interval predictions.

* `age_at_maturity` is replaced by `ages_movement`. The former can take either a
  single integer indicating the age at which individuals start to move, or a
  vector with 0s for age-groups that do not move and 1s for age-groups that are
  allowed to move.

# drmr 0.0.23

* The prior for $\alpha$ has been modified. In particular, instead of a pcp
  prior, now we put a Beta prior on $(\alpha + 1) / 2$. The hyperparameters of
  this Beta distribution are `pr_alpha_a` and `pr_alpha_b`, respectively.
   
* New functions to initialize the parameters from the prior have been
  introduced. Now the `init` parmeter from the `fit_sdm` and `fit_drm` functions
  can take three possible values:
  * "default": the standard initialization in `Stan` (For details see [`Stan's
    documentation`](https://mc-stan.org/docs/reference-manual/execution.html#random-initial-values)).
  * "prior": initialize the parameters using samples from their respective prior
    distributions;
  * "pathfinder": uses the
    [Pathfinder](https://mc-stan.org/docs/reference-manual/pathfinder.html)
    algorithm to initialize the parameters.

* Fixed some issues with documentation.

# drmr 0.0.22

* `fit_drm` and `fit_sdm` functions to make model fitting slightly simpler.

* `pr_phi_a` and `pr_phi_b` become `pr_phi_mu` and `pr_phi_sd`. The prior for
  $\log(\phi)$ is a Student's t with 3 degrees of freedom, mean `pr_phi_mu` and
  SD `pr_phi_sd`.
   
* Selectivity in `make_data` was fixed, thanks to Mark. Before, it was not being
  used when users input it.

# drmr 0.0.21

* `make_data_sdm` function (analogous to `make_data`) created for SDM.

* `predict_sdm` function (analogous to `predict_drm`) created for SDM.

# drmr 0.0.2

* Parameters in the code and documentation were properly matched.

* `p_error` toggle becomes `time_ar` toggle (more appropriate).

* `predict_drm` function created

* the `make_data` function now has a `family` argument indicating the
  probability distribution assumed for the response (given all the model
  parameters and latent variables)

# drmr 0.0.1

* First version of the package containing pre-compiled code for fitting the DRM,
  making forecasts and calculating density weighted centroids.
