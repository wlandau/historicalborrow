# Mixture model MCMC hyperparameters

Run a simple model separately on each historical study control group and
compute hyperparameters for
[`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md).

## Usage

``` r
hb_mcmc_mixture_hyperparameters(
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient",
  m_mu = 0,
  s_mu = 30,
  s_sigma = 30,
  m_omega_current = 0,
  s_omega_current = 30,
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

- m_mu:

  Numeric of length 1, prior mean of the mean `mu` in the simple model.

- s_mu:

  Numeric of length 1, prior standard deviation of the mean `mu` in the
  simple model.

- s_sigma:

  Numeric of length 1, uniform prior upper bound of the residual
  standard deviation `sigma` in the simple model.

- m_omega_current:

  Numeric with length 1, `m_omega` value of the current study. Inserted
  as the final component of the `m_omega` column in the output.

- s_omega_current:

  Numeric with length 1, `s_omega` value of the current study. Inserted
  as the final component of the `s_omega` column in the output.

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

A tidy data frame of hyperparameter values for
[`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md).
The first several rows are for historical studies, and the last row is
for the current study. Studies/rows are sorted in the order
[`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md)
sorts them, so you can use columns `m_omega` and `s_omega` for the same
dataset and same values of other arguments directly in
[`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md).

## Details

The model is a simple Bayesian model with a normal likelihood, an
unknown mean `mu`, and an unknown standard deviation `sigma`. For each
historical study, the posterior mean of `mu` becomes the corresponding
component of `m_omega` in the output, and the posterior standard
deviation of `mu` becomes the corresponding component of `s_omega` in
the output. See the examples in this help file for a demonstration.
`m_omega` and `s_omega` define the components of the mixture prior in
[`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md)
that act as the contribution of the historical studies to the model.

## See also

Other mcmc:
[`hb_convergence()`](https://wlandau.github.io/historicalborrow/reference/hb_convergence.md),
[`hb_mcmc_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_hierarchical.md),
[`hb_mcmc_independent()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_independent.md),
[`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md),
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
#> 1 study1           1  0.342   0.182 
#> 2 study2           2  0.0206  0.0374
#> 3 study3           3 -0.855   0.0948
#> 4 study4           4 -0.456   0.288 
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
#>    alpha `beta[1]` `beta[2]` `delta[1]` `delta[2]` `omega[1]` `omega[2]`
#>    <dbl>     <dbl>     <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1 -1.54     0.603      1.14      0.599     -0.609    -0.0702    0.0582 
#>  2 -1.46     0.553      1.06      0.587     -0.611     0.243     0.0244 
#>  3 -1.57     0.638      1.09      0.539     -0.580     0.357     0.0671 
#>  4 -1.65     0.560      1.22      0.621     -0.623     0.0955    0.119  
#>  5 -1.51     0.687      1.03      0.622     -0.576     0.411     0.0461 
#>  6 -1.66     0.640      1.11      0.654     -0.585     0.241     0.0489 
#>  7 -1.58     0.572      1.16      0.606     -0.617     0.368     0.0281 
#>  8 -1.68     0.566      1.20      0.473     -0.631     0.456     0.00625
#>  9 -1.61     0.643      1.09      0.346     -0.715     0.295     0.0467 
#> 10 -1.67     0.613      1.05      0.607     -0.658     0.488    -0.0488 
#> # ℹ 40 more rows
#> # ℹ 12 more variables: `omega[3]` <dbl>, `omega[4]` <dbl>, `omega[5]` <dbl>,
#> #   `post_p[1]` <dbl>, `post_p[2]` <dbl>, `post_p[3]` <dbl>, `post_p[4]` <dbl>,
#> #   `post_p[5]` <dbl>, sigma <dbl>, .chain <int>, .iteration <int>, .draw <int>
```
