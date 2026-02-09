# Check convergence diagnostics

Check the convergence diagnostics on a model.

## Usage

``` r
hb_convergence(mcmc)
```

## Arguments

- mcmc:

  A wide data frame of posterior samples returned by
  [`hb_mcmc_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_hierarchical.md)
  or similar MCMC function.

## Value

A data frame of summarized convergence diagnostics. `max_rhat` is the
maximum univariate Gelman/Rubin potential scale reduction factor over
all the parameters of the model, `min_ess_bulk` is the minimum bulk
effective sample size over the parameters, and `min_ess_tail` is the
minimum tail effective sample size. `max_rhat` should be below 1.01, and
the ESS metrics should both be above 100 times the number of MCMC
chains. If any of these conditions are not true, the MCMC did not
converge, and it is recommended to try running the model for more saved
iterations (and if `max_rhat` is high, possibly more warmup iterations).

## See also

Other mcmc:
[`hb_mcmc_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_hierarchical.md),
[`hb_mcmc_independent()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_independent.md),
[`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md),
[`hb_mcmc_mixture_hyperparameters()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture_hyperparameters.md),
[`hb_mcmc_pool()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_pool.md)

## Examples

``` r
data <- hb_sim_pool(n_continuous = 2)$data
mcmc <- hb_mcmc_pool(
  data,
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 200,
  n_iterations = 200
)
hb_convergence(mcmc)
#> # A tibble: 1 × 3
#>   max_rhat min_ess_bulk min_ess_tail
#>      <dbl>        <dbl>        <dbl>
#> 1     1.03         123.         87.7
```
