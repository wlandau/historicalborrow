
# historicalborrow

Historical borrowing in clinical trials can improve precision and
operating characteristics. This package supports a hierarchical model
and a mixture model to borrow historical control data from other studies
to better characterize the response of the control arm of the current
study. It also quantifies the amount of borrowing through benchmark
models (independent and pooled). Some of the methods are discussed by
Viele et al. (2013).

## Installation

``` r
remotes::install_github("wlandau/historicalborrow")
```

## Documentation

- Functions: <https://wlandau.github.io/historicalborrow/reference/>
- Methods:
  <https://wlandau.github.io/historicalborrow/articles/methods.html>
- Usage:
  <https://wlandau.github.io/historicalborrow/articles/usage.html>

## Thanks

[Albert Man](https://github.com/albert-man) reviewed this package.
Albert, [Faith Bian](https://github.com/faithbian-lilly), and [Saptarshi
Chatterjee](https://github.com/schatterjee-lilly) contributed to the
development of the methods prior to implementation. Phebe Kemmer,
Heather Zhao, and Zhangchen Zhao reviewed and provided helpful feedback.

## Code of Conduct

Please note that the historicalborrow project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## References

- Viele, Kert and Berry, Scott and Neuenschwander, Beat et al. “Use of
  historical control data for assessing treatment effects in clinical
  trials.” Pharmaceutical Statistics 1(13), 2013.
