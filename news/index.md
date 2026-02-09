# Changelog

## historicalborrow 1.1.1

- Run
  [`hb_ess()`](https://wlandau.github.io/historicalborrow/reference/hb_ess.md)
  tests longer in preparation for JAGS 5.0.0
  ([\#11](https://github.com/wlandau/historicalborrow/issues/11),
  [@martynplummer](https://github.com/martynplummer)).

## historicalborrow 1.1.0

CRAN release: 2024-09-10

- Avoid `aes_string()`.
- Default to half-Student-t priors for hierarchical model MCMC and
  simulation
  ([\#6](https://github.com/wlandau/historicalborrow/issues/6)).
- Use proportional averaging instead of cell-level averaging for
  estimating marginal means
  ([\#7](https://github.com/wlandau/historicalborrow/issues/7)).
- Add
  [`hb_ess()`](https://wlandau.github.io/historicalborrow/reference/hb_ess.md),
  which supersedes
  [`hb_metrics()`](https://wlandau.github.io/historicalborrow/reference/hb_metrics.md)
  ([\#8](https://github.com/wlandau/historicalborrow/issues/8)).

## historicalborrow 1.0.4

CRAN release: 2022-09-13

- Fix DOI formatting in the `DESCRIPTION` file.

## historicalborrow 1.0.3

- First open-source release.

## historicalborrow 1.0.2

- Fix serious bug in data confidence interval calculation in
  [`hb_summary()`](https://wlandau.github.io/historicalborrow/reference/hb_summary.md).

## historicalborrow 1.0.1

- Use `n` in
  [`hb_s_tau()`](https://wlandau.github.io/historicalborrow/reference/hb_s_tau.md)
  ([\#13](https://github.com/wlandau/historicalborrow/issues/13)).
- Remove missing patients in `n_study_current` for the precision ratio
  ([\#14](https://github.com/wlandau/historicalborrow/issues/14)).
- Added a `NEWS.md` file to track changes to the package.
