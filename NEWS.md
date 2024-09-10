# historicalborrow 1.1.0.9000 (development)



# historicalborrow 1.1.0

* Avoid `aes_string()`.
* Default to half-Student-t priors for hierarchical model MCMC and simulation (#6).
* Use proportional averaging instead of cell-level averaging for estimating marginal means (#7).
* Add `hb_ess()`, which supersedes `hb_metrics()` (#8).

# historicalborrow 1.0.4

* Fix DOI formatting in the `DESCRIPTION` file.

# historicalborrow 1.0.3

* First open-source release.

# historicalborrow 1.0.2

* Fix serious bug in data confidence interval calculation in `hb_summary()`.

# historicalborrow 1.0.1

* Use `n` in `hb_s_tau()` (#13).
* Remove missing patients in `n_study_current` for the precision ratio (#14).
* Added a `NEWS.md` file to track changes to the package.
