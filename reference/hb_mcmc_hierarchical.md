# Hierarchical model MCMC

Run the hierarchical model with MCMC.

## Usage

``` r
hb_mcmc_hierarchical(
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient",
  covariates = grep("^covariate", colnames(data), value = TRUE),
  s_delta = 30,
  s_beta = 30,
  s_sigma = 30,
  s_mu = 30,
  s_tau = sd(data[[response]], na.rm = TRUE),
  d_tau = 1,
  prior_tau = "half_t",
  n_chains = 4,
  n_adapt = 2000,
  n_warmup = 4000,
  n_iterations = 20000,
  quiet = TRUE
)
```

## Arguments

- data:

  Tidy data frame with one row per patient, indicator columns for the
  response variable, study, group, and patient, and covariates. All
  columns must be atomic vectors (e.g. not lists). The data for the
  mixture and simple models should have just one study, and the others
  should have data from more than one study. The simple model can be
  used to get the historical data components of `m_omega` and `s_omega`
  for the mixture model.

- response:

  Character of length 1, name of the column in `data` with the
  response/outcome variable. `data[[response]]` must be a continuous
  variable, and it *should* be the change from baseline of a clinical
  endpoint of interest, as opposed to just the raw response. Treatment
  differences are computed directly from this scale, please supply
  change from baseline unless you are absolutely certain that treatment
  differences computed directly from this quantity are clinically
  meaningful.

- study:

  Character of length 1, name of the column in `data` with the study ID.

- study_reference:

  Atomic of length 1, element of the `study` column that indicates the
  current study. (The other studies are historical studies.)

- group:

  Character of length 1, name of the column in `data` with the group ID.

- group_reference:

  Atomic of length 1, element of the `group` column that indicates the
  control group. (The other groups may be treatment groups.)

- patient:

  Character of length 1, name of the column in `data` with the patient
  ID.

- covariates:

  Character vector of column names in `data` with the columns with
  baseline covariates. These can be continuous, categorical, or binary.
  Regardless, `historicalborrow` derives the appropriate model matrix.

- s_delta:

  Numeric of length 1, prior standard deviation of the study-by-group
  effect parameters `delta`.

- s_beta:

  Numeric of length 1, prior standard deviation of the fixed effects
  `beta`.

- s_sigma:

  Numeric of length 1, prior upper bound of the residual standard
  deviations.

- s_mu:

  Numeric of length 1, prior standard deviation of `mu`.

- s_tau:

  Non-negative numeric of length 1. If `prior_tau` is `"half_t"`, then
  `s_tau` is the scale parameter of the Student t prior of `tau` and
  analogous to the `sigma` parameter of the Student-t parameterization
  given at
  <https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html>.
  \# nolint If `prior_tau` is `"uniform"`, then `s_tau` is the upper
  bound of `tau`. Upper bound on `tau` if `prior_tau` is `"uniform"`.

  In the case of `prior_tau` equal to `"half_t"`, the defaults
  `s_tau = sd(data[[response]], na.rm = TRUE)` and `d_tau = 1` specify a
  weakly informative scaled half-Cauchy distribution. This choice is
  only provisional. The prior on `tau` is extremely important,
  especially for small numbers of historical studies, and the user
  should set a reasonable value for the use case, ideally informed by
  the results of sensitivity analyses and simulations.

- d_tau:

  Positive numeric of length 1. Degrees of freedom of the Student t
  prior of `tau` if `prior_tau` is `"half_t"`.

  In the case of `prior_tau` equal to `"half_t"`, the defaults
  `s_tau = sd(data[[response]], na.rm = TRUE)` and `d_tau = 1` specify a
  weakly informative scaled half-Cauchy distribution. This choice is
  only provisional. The prior on `tau` is extremely important,
  especially for small numbers of historical studies, and the user
  should set a reasonable value for the use case, ideally informed by
  the results of sensitivity analyses and simulations.

- prior_tau:

  Character string, family of the prior of `tau`. If `prior_tau` equals
  `"uniform"`, then the prior on `tau` is a uniform prior with lower
  bound 0 and upper bound `s_tau`. If `prior_tau` equals `"half_t"`,
  then the prior on `tau` is a half Student-t prior with center 0, lower
  bound 0, scale parameter `s_tau`, and degrees of freedom `d_tau`. The
  scale parameter `s_tau` is analogous to the `sigma` parameter of the
  Student-t parameterization given at
  <https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html>.
  \# nolint

- n_chains:

  Number of MCMC chains to run.

- n_adapt:

  Number of adaptation iterations to run.

- n_warmup:

  Number of warmup iterations per chain to run.

- n_iterations:

  Number of saved MCMC iterations per chain to run.

- quiet:

  Logical of length 1, `TRUE` to suppress R console output.

## Value

A tidy data frame of parameter samples from the posterior distribution.
Columns `.chain`, `.iteration`, and `.draw` have the meanings documented
in the `posterior` package.

## See also

Other mcmc:
[`hb_convergence()`](https://wlandau.github.io/historicalborrow/reference/hb_convergence.md),
[`hb_mcmc_independent()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_independent.md),
[`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md),
[`hb_mcmc_mixture_hyperparameters()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture_hyperparameters.md),
[`hb_mcmc_pool()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_pool.md)

## Examples

``` r
if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
data <- hb_sim_hierarchical(n_continuous = 2)$data
hb_mcmc_hierarchical(
  data,
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 50,
  n_iterations = 50
)
}
```
