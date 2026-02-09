# Non-longitudinal hierarchical simulations.

Simulate from the non-longitudinal hierarchical model.

## Usage

``` r
hb_sim_hierarchical(
  n_study = 5,
  n_group = 3,
  n_patient = 100,
  n_continuous = 0,
  n_binary = 0,
  s_delta = 1,
  s_beta = 1,
  s_sigma = 1,
  s_mu = 1,
  s_tau = 1,
  d_tau = 4,
  prior_tau = "half_t",
  alpha = NULL,
  delta = stats::rnorm(n = n_group - 1, mean = 0, sd = s_delta),
  beta = stats::rnorm(n = n_study * (n_continuous + n_binary), mean = 0, sd = s_delta),
  sigma = stats::runif(n = n_study, min = 0, max = s_sigma),
  mu = stats::rnorm(n = 1, mean = 0, sd = s_mu),
  tau = NULL
)
```

## Arguments

- n_study:

  Number of studies to simulate.

- n_group:

  Number of groups (e.g. study arms) to simulate per study.

- n_patient:

  Number of patients to simulate per study per group.

- n_continuous:

  Number of continuous covariates to simulate (all from independent
  standard normal distributions).

- n_binary:

  Number of binary covariates to simulate (all from independent
  Bernoulli distributions with p = 0.5).

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

- d_tau:

  Positive numeric of length 1. Degrees of freedom of the Student t
  prior of `tau` if `prior_tau` is `"half_t"`.

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

- alpha:

  Numeric vector of length 1 for the pooled and mixture models and
  length `n_study` for the independent and hierarchical models. `alpha`
  is the vector of control group mean parameters. `alpha` enters the
  model by multiplying with `$matrices$x_alpha` (see the return value).
  The control group in the data is the one with the `group` column equal
  to 1.

- delta:

  Numeric vector of length `n_group - 1` of treatment effect parameters.
  `delta` enters the model by multiplying with `$matrices$x_delta` (see
  the return value). The control (non-treatment) group in the data is
  the one with the `group` column equal to 1.

- beta:

  Numeric vector of `n_study * (n_continuous + n_binary)` fixed effect
  parameters. Within each study, the first `n_continuous` betas are for
  the continuous covariates, and the rest are for the binary covariates.
  All the `beta`s for one study appear before all the `beta`s for the
  next study, and studies are arranged in increasing order of the sorted
  unique values in `$data$study` in the output. `betas` enters the model
  by multiplying with `$matrices$x_alpha` (see the return value).

- sigma:

  Numeric vector of `n_study` study-specific residual standard
  deviations.

- mu:

  Numeric of length 1, mean of the control group means `alpha`.

- tau:

  Numeric of length 1, standard deviation of the control group means
  `alpha`.

## Value

A list with the following elements:

- `data`: tidy long-form dataset with the patient-level data. one row
  per patient and indicator columns for the study, group (e.g. treatment
  arm), and patient ID. The `response` columns is the patient response.
  The other columns are baseline covariates. The control group is the
  one with the `group` column equal to 1, and the current study
  (non-historical) is the one with the maximum value of the `study`
  column. Only the current study has any non-control-group patients, the
  historical studies have only the control group.

- `parameters`: named list of model parameter values. See the model
  specification vignette for details.

- `matrices`: A named list of model matrices. See the model
  specification vignette for details.

## See also

Other simulate:
[`hb_sim_independent()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_independent.md),
[`hb_sim_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_mixture.md),
[`hb_sim_pool()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_pool.md)

## Examples

``` r
hb_sim_hierarchical()$data
#> # A tibble: 700 × 4
#>    study group patient response
#>    <int> <int>   <int>    <dbl>
#>  1     1     1       1     1.29
#>  2     1     1       2     1.49
#>  3     1     1       3     1.36
#>  4     1     1       4     1.56
#>  5     1     1       5     1.75
#>  6     1     1       6     1.65
#>  7     1     1       7     1.73
#>  8     1     1       8     1.44
#>  9     1     1       9     1.45
#> 10     1     1      10     1.44
#> # ℹ 690 more rows
```
