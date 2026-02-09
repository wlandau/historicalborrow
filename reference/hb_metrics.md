# Legacy function to calculate borrowing metrics

Legacy function to calculate historical borrowing metrics using summary
output from a fitted borrowing model and analogous summaries from the
benchmark models. We recommend
[`hb_ess()`](https://wlandau.github.io/historicalborrow/reference/hb_ess.md)
instead of `hb_metrics()`. See the methods vignette in the package for
details.

## Usage

``` r
hb_metrics(borrow, pool, independent)
```

## Arguments

- borrow:

  A data frame returned by
  [`hb_summary()`](https://wlandau.github.io/historicalborrow/reference/hb_summary.md)
  for the mixture or hierarchical model.

- pool:

  A data frame returned by
  [`hb_summary()`](https://wlandau.github.io/historicalborrow/reference/hb_summary.md)
  for the pooled model.

- independent:

  A data frame returned by
  [`hb_summary()`](https://wlandau.github.io/historicalborrow/reference/hb_summary.md)
  for the independent model.

## Value

A data frame with borrowing metrics.

## Examples

``` r
if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
data <- hb_sim_independent(n_continuous = 2)$data
mcmc_borrow <- hb_mcmc_hierarchical(
  data,
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 100,
  n_iterations = 200
)
mcmc_pool <- hb_mcmc_pool(
  data,
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 50,
  n_iterations = 50
)
mcmc_independent <- hb_mcmc_independent(
  data,
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 50,
  n_iterations = 50
)
borrow <- hb_summary(mcmc_borrow, data)
pool <- hb_summary(mcmc_pool, data)
independent <- hb_summary(mcmc_independent, data)
hb_metrics(
  borrow = borrow,
  pool = pool,
  independent = independent
)
}
```
