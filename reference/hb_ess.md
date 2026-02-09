# Effective sample size (ESS)

Quantify borrowing with effective sample size (ESS) as cited and
explained in the methods vignette at
<https://wlandau.github.io/historicalborrow/articles/methods.html>.

## Usage

``` r
hb_ess(
  mcmc_pool,
  mcmc_hierarchical,
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient"
)
```

## Arguments

- mcmc_pool:

  A fitted model from
  [`hb_mcmc_pool()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_pool.md).

- mcmc_hierarchical:

  A fitted model from
  [`hb_mcmc_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_hierarchical.md).

- data:

  A tidy data frame or `tibble` with the data.

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

## Value

A data frame with one row and the following columns:

- `v0`: posterior predictive variance of the control group mean of a
  hypothetical new study given the pooled model. Calculated as the mean
  over MCMC samples of `1 / sum(sigma_i ^ 2)`, where each `sigma_i` is
  the residual standard deviation of study `i` estimated from the pooled
  model.

- `v_tau`: posterior predictive variance of a hypothetical new control
  group mean under the hierarchical model. Calculated by averaging over
  predictive draws, where each predictive draw is from
  `rnorm(n = 1, mean = mu_, sd = tau_)` and `mu_` and `tau_` are the
  `mu` and `tau` components of an MCMC sample.

- `n`: number of non-missing historical control patients.

- `weight`: strength of borrowing as a ratio of variances: `v0 / v_tau`.

- `ess`: strength of borrowing as an effective sample size:
  `n v0 / v_tau`, where `n` is the number of non-missing historical
  control patients.

## See also

Other summary:
[`hb_summary()`](https://wlandau.github.io/historicalborrow/reference/hb_summary.md)

## Examples

``` r
  data <- hb_sim_independent(n_continuous = 2)$data
  data$group <- sprintf("group%s", data$group)
  data$study <- sprintf("study%s", data$study)
  pool <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  hierarchical <- hb_mcmc_hierarchical(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  hb_ess(
    mcmc_pool = pool,
    mcmc_hierarchical = hierarchical,
    data = data
  )
#> # A tibble: 1 × 5
#>     ess weight     n     v0 v_tau
#>   <dbl>  <dbl> <int>  <dbl> <dbl>
#> 1  4.75 0.0119   400 0.0767  6.46
```
