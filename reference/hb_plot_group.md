# Plot the groups of a borrowing model and its benchmark models.

Plot the groups against one another for a borrowing model (hierarchical
or mixture) and the independent and pooled benchmark models.

## Usage

``` r
hb_plot_group(borrow, pool, independent, outcome = c("response", "diff"))
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

- outcome:

  Character of length 1, either `"response"` or `"diff"`, the quantity
  to plot on the vertical axis.

## Value

A `ggplot` object

## See also

Other plot:
[`hb_plot_borrow()`](https://wlandau.github.io/historicalborrow/reference/hb_plot_borrow.md),
[`hb_plot_tau()`](https://wlandau.github.io/historicalborrow/reference/hb_plot_tau.md)

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
  n_warmup = 200,
  n_iterations = 200
)
mcmc_independent <- hb_mcmc_independent(
  data,
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 200,
  n_iterations = 200
)
borrow <- hb_summary(mcmc_borrow, data)
pool <- hb_summary(mcmc_pool, data)
independent <- hb_summary(mcmc_independent, data)
hb_plot_group(
  borrow = borrow,
  pool = pool,
  independent = independent
)
}
```
