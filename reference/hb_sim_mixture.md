# Non-longitudinal mixture simulations.

Simulate from the non-longitudinal mixture model.

## Usage

``` r
hb_sim_mixture(
  n_study = 5,
  n_group = 3,
  n_patient = 100,
  n_continuous = 0,
  n_binary = 0,
  s_delta = 1,
  s_beta = 1,
  s_sigma = 1,
  m_omega = 0,
  s_omega = 1,
  p_omega = 1/n_study,
  alpha = omega[pi],
  delta = stats::rnorm(n = n_group - 1, mean = 0, sd = s_delta),
  beta = stats::rnorm(n = n_continuous + n_binary, mean = 0, sd = s_delta),
  sigma = stats::runif(n = 1, min = 0, max = s_sigma),
  pi = sample.int(n = n_study, size = 1, prob = p_omega),
  omega = stats::rnorm(n = n_study, mean = m_omega, sd = s_omega)
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

- m_omega:

  Numeric of length 1 or `n_study`, prior control group mean of each
  study. If length `n_study`, then the last element corresponds to the
  current study, and the others are for historical studies.

- s_omega:

  Numeric of length 1 or `n_study`, prior control group standard
  deviation of each study. If length `n_study`, the the last element
  corresponds to the current study, and the others are for historical
  studies.

- p_omega:

  Numeric of length `n_study`, prior mixture proportion of each study.
  If length `n_study`, then the last element corresponds to the current
  study, and the others are for historical studies.

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

  Numeric vector of `n_continuous + n_binary` fixed effect parameters.
  The first `n_continuous` betas are for the continuous covariates, and
  the rest are for the binary covariates. `betas` enters the model by
  multiplying with `$matrices$x_alpha` (see the return value).

- sigma:

  Numeric vector of `n_study` study-specific residual standard
  deviations.

- pi:

  Integer of length 1, index of the mixture component randomly chosen
  for `alpha`.

- omega:

  Numeric of length `n_study`, Candidate placebo mean parameters drawn
  from each of the mixture components.

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
[`hb_sim_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_hierarchical.md),
[`hb_sim_independent()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_independent.md),
[`hb_sim_pool()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_pool.md)

## Examples

``` r
hb_sim_mixture()$data
#> # A tibble: 300 × 4
#>    study group patient response
#>    <int> <int>   <int>    <dbl>
#>  1     1     1       1    0.890
#>  2     1     1       2    0.864
#>  3     1     1       3    1.79 
#>  4     1     1       4    1.000
#>  5     1     1       5    1.20 
#>  6     1     1       6    0.973
#>  7     1     1       7    1.16 
#>  8     1     1       8    1.34 
#>  9     1     1       9    1.14 
#> 10     1     1      10    0.871
#> # ℹ 290 more rows
```
