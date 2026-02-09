# Mixture model MCMC

Run the mixture model with MCMC.

## Usage

``` r
hb_mcmc_mixture(
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
  m_omega = c(0, 0),
  s_omega = c(30, 30),
  p_omega = 1/length(m_omega),
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

- m_omega:

  Numeric with length equal to the number of supposed studies (but only
  the current study is in the data). `m_omega` is the prior control
  group mean of each study. The last element corresponds to the current
  study, and the others are for historical studies.

- s_omega:

  Numeric with length equal to the number of supposed studies (but only
  the current study is in the data). `s_omega` is the prior control
  group standard deviation of each study. The last element corresponds
  to the current study, and the others are for historical studies.

- p_omega:

  Numeric with length equal to the number of supposed studies (but only
  the current study is in the data). `p_omega` is the prior control
  group mixture proportion of each study. The last element corresponds
  to the current study, and the others are for historical studies.

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

## Details

The study-specific components of the mixture prior are all fixed in
advance. Mixture components are normal distributions with means in
`m_omega` and standard deviations in `s_omega`. These vectors are
ordered with historical studies first and the current study last. These
mixture components can be computed using
[`hb_mcmc_mixture_hyperparameters()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture_hyperparameters.md)
on a full set of data (all the historical studies and the current study
together). Then the `m_omega` and `s_omega` columns of the output can be
plugged directly into `hb_mcmc_mixture()`. See the examples for a
demonstration.

## See also

Other mcmc:
[`hb_convergence()`](https://wlandau.github.io/historicalborrow/reference/hb_convergence.md),
[`hb_mcmc_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_hierarchical.md),
[`hb_mcmc_independent()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_independent.md),
[`hb_mcmc_mixture_hyperparameters()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture_hyperparameters.md),
[`hb_mcmc_pool()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_pool.md)

## Examples

``` r
data_all_studies <- hb_sim_independent(n_continuous = 2)$data
data_all_studies$study <- paste0("study", data_all_studies$study)
hyperparameters <- hb_mcmc_mixture_hyperparameters(
  data = data_all_studies,
  response = "response",
  study = "study",
  study_reference = "study5",
  group = "group",
  group_reference = 1,
  patient = "patient",
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 50,
  n_iterations = 50
)
print(hyperparameters)
#> # A tibble: 5 × 4
#>   study  study_index m_omega s_omega
#>   <chr>        <int>   <dbl>   <dbl>
#> 1 study1           1  0.0833  0.0703
#> 2 study2           2 -0.226   0.156 
#> 3 study3           3 -0.267   0.139 
#> 4 study4           4 -2.43    0.219 
#> 5 study5           5  0      30     
data_current_study <- dplyr::filter(data_all_studies, study == max(study))
hb_mcmc_mixture(
  data = data_current_study,
  response = "response",
  study = "study",
  study_reference = "study5",
  group = "group",
  group_reference = 1,
  patient = "patient",
  m_omega = hyperparameters$m_omega, # use hyperparams from historical data
  s_omega = hyperparameters$s_omega, # use hyperparams from historical data
  p_omega = rep(1 / nrow(hyperparameters), nrow(hyperparameters)),
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 50,
  n_iterations = 50
)
#> # A tibble: 50 × 19
#>     alpha `beta[1]` `beta[2]` `delta[1]` `delta[2]` `omega[1]` `omega[2]`
#>     <dbl>     <dbl>     <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1 -0.295    0.0309     0.295     -0.361     -0.799   -0.00489     -0.295
#>  2 -0.310    0.0289     0.289     -0.375     -0.799    0.0474      -0.310
#>  3 -0.288    0.0335     0.295     -0.396     -0.794    0.167       -0.288
#>  4 -0.288    0.0424     0.291     -0.401     -0.778    0.102       -0.288
#>  5 -0.346    0.0240     0.283     -0.389     -0.801   -0.0243      -0.346
#>  6 -0.300    0.0355     0.288     -0.346     -0.789    0.0631      -0.300
#>  7 -0.303    0.0413     0.277     -0.374     -0.813    0.109       -0.303
#>  8 -0.296    0.0313     0.299     -0.376     -0.781    0.0683      -0.296
#>  9 -0.299    0.0405     0.299     -0.390     -0.793    0.123       -0.299
#> 10 -0.287    0.0377     0.296     -0.357     -0.787    0.119       -0.287
#> # ℹ 40 more rows
#> # ℹ 12 more variables: `omega[3]` <dbl>, `omega[4]` <dbl>, `omega[5]` <dbl>,
#> #   `post_p[1]` <dbl>, `post_p[2]` <dbl>, `post_p[3]` <dbl>, `post_p[4]` <dbl>,
#> #   `post_p[5]` <dbl>, sigma <dbl>, .chain <int>, .iteration <int>, .draw <int>
```
