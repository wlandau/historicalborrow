# Non-longitudinal pooled simulations.

Simulate from the non-longitudinal pooled model.

## Usage

``` r
hb_sim_pool(
  n_study = 5,
  n_group = 3,
  n_patient = 100,
  n_continuous = 0,
  n_binary = 0,
  s_alpha = 1,
  s_delta = 1,
  s_beta = 1,
  s_sigma = 1,
  alpha = stats::rnorm(n = 1, mean = 0, sd = s_alpha),
  delta = stats::rnorm(n = n_group - 1, mean = 0, sd = s_delta),
  beta = stats::rnorm(n = n_study * (n_continuous + n_binary), mean = 0, sd = s_delta),
  sigma = stats::runif(n = n_study, min = 0, max = s_sigma)
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

- s_alpha:

  Numeric of length 1, prior standard deviation of the study-specific
  control group mean parameters `alpha`.

- s_delta:

  Numeric of length 1, prior standard deviation of the study-by-group
  effect parameters `delta`.

- s_beta:

  Numeric of length 1, prior standard deviation of the fixed effects
  `beta`.

- s_sigma:

  Numeric of length 1, prior upper bound of the residual standard
  deviations.

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
[`hb_sim_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_mixture.md)

## Examples

``` r
hb_sim_pool(n_continuous = 1)$data
#> # A tibble: 700 × 9
#>    study group patient covariate_study1_continuous1 covariate_study2_continuous1
#>    <int> <int>   <int>                        <dbl>                        <dbl>
#>  1     1     1       1                      -0.836                             0
#>  2     1     1       2                      -0.669                             0
#>  3     1     1       3                      -0.990                             0
#>  4     1     1       4                       1.57                              0
#>  5     1     1       5                       2.93                              0
#>  6     1     1       6                       0.284                             0
#>  7     1     1       7                       0.572                             0
#>  8     1     1       8                       0.0726                            0
#>  9     1     1       9                      -1.11                              0
#> 10     1     1      10                       1.11                              0
#> # ℹ 690 more rows
#> # ℹ 4 more variables: covariate_study3_continuous1 <dbl>,
#> #   covariate_study4_continuous1 <dbl>, covariate_study5_continuous1 <dbl>,
#> #   response <dbl>
```
