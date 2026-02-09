# Superseded suggest s_tau given a uniform prior on tau.

For the uniform prior on tau, suggest a value of the `s_tau`
hyperparameter to achieve a given amount of borrowing in the
hierarchical model. Only use if a diffuse prior on `tau` is not
feasible.

## Usage

``` r
hb_s_tau(precision_ratio = 0.5, sigma = 1, n = 100)
```

## Arguments

- precision_ratio:

  Positive numeric vector of elements between 0 and 1 with target
  precision ratios.

- sigma:

  Positive numeric vector of residual standard deviations.

- n:

  Number of non-missing patients.

## Value

Numeric of length equal to `length(precision_ratio)` and
`length(sigma)`, suggested values of s_tau for each element of
`precision_ratio` and `sigma`.

## Details

See the hierarchical model section of the methods vignette for details.

## Examples

``` r
hb_s_tau(precision_ratio = 0.5, sigma = 1, n = 100)
#> [1] 0.2
```
