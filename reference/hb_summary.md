# Model summary

Summarize a fitted model in a table.

## Usage

``` r
hb_summary(
  mcmc,
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient",
  covariates = grep("^covariate", colnames(data), value = TRUE),
  eoi = 0,
  direction = "<"
)
```

## Arguments

- mcmc:

  A wide data frame of posterior samples returned by
  [`hb_mcmc_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_hierarchical.md)
  or similar MCMC function.

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

- eoi:

  Numeric of length at least 1, vector of effects of interest (EOIs) for
  critical success factors (CSFs).

- direction:

  Character of length `length(eoi)` indicating how to compare the
  treatment effect to each EOI. `">"` means Prob(treatment effect \>
  EOI), and `"<"` means Prob(treatment effect \< EOI). All elements of
  `direction` must be either `">"` or `"<"`.

## Value

A tidy data frame with one row per group (e.g. treatment arm) and the
columns in the following list. Unless otherwise specified, the
quantities are calculated at the group level. Some are calculated for
the current (non-historical) study only, while others pertain to the
combined dataset which includes all historical studies. The mixture
model is an exception because the `data` argument only includes the
current study, so other quantities that include historical information
will need to borrow from an `hb_summary()` call on one of the other
models.

- `group`: group label.

- `data_mean`: observed mean response specific to the current study.

- `data_sd`: observed standard deviation of the response specific to the
  current study.

- `data_lower`: lower bound of a simple frequentist 95% confidence
  interval of the observed mean specific to the current study.

- `data_upper`: upper bound of a simple frequentist 95% confidence
  interval of the observed mean specific to the current study.

- `data_n`: number of non-missing observations in the combined dataset
  with all studies.

- `data_N`: total number of observations (missing and non-missing) in
  the combined dataset with all studies.

- `data_n_study_*`: number of non-missing observations separately for
  each study. The suffixes of these column names are integer study
  indexes. Call
  `dplyr::distinct(hb_data(your_data), study, study_label)` to see which
  study labels correspond to these integer indexes. Note: the combined
  dataset for the mixture model is just the current study. If all the
  `data_n_study_*` results across all studies are desired, then call
  `hb_summary()` on a different model (e.g. pooled).

- `data_N_study_*`: same as `data_n_study_*` except both missing and
  non-missing observations are counted (total number of observations).

- `response_mean`: Estimated posterior mean of the response from the
  model specific to the current study. Typically, the raw response is
  change from baseline, in which case `response_mean` is estimating
  change from baseline.

- `response_sd`: Estimated posterior standard deviation of the mean
  response from the model specific to the current study.

- `response_variance`: Estimated posterior variance of the mean response
  from the model specific to the current study.

- `response_lower`: Lower bound of a 95% posterior interval on the mean
  response from the model specific to the current study.

- `response_upper`: Upper bound of a 95% posterior interval on the mean
  response from the model specific to the current study.

- `response_mean_mcse`: Monte Carlo standard error of `response_mean`.

- `response_sd_mcse`: Monte Carlo standard error of `response_sd`.

- `response_lower_mcse`: Monte Carlo standard error of `response_lower`.

- `response_upper_mcse`: Monte Carlo standard error of `response_upper`.

- `diff_mean`: Estimated treatment effect from the model specific to the
  current study.

- `diff_lower`: Lower bound of a 95% posterior interval on the treatment
  effect from the model specific to the current study..

- `diff_upper`: Upper bound of a 95% posterior interval on the treatment
  effect from the model specific to the current study..

- `diff_mean_mcse`: Monte Carlo standard error of `diff_mean`.

- `diff_lower_mcse`: Monte Carlo standard error of `diff_lower`.

- `diff_upper_mcse`: Monte Carlo standard error of `diff_upper`.

- `P(diff > EOI)`, `P(diff < EOI)`: CSF probabilities on the treatment
  effect specified with the `eoi` and `direction` arguments. Specific to
  the current study.

- `effect_mean`: Estimated posterior mean of effect size (treatment
  difference divided by residual standard deviation). Specific to the
  current study.

- `effect_lower`: Lower bound of a 95% posterior interval of effect size
  from the model. Specific to the current study.

- `effect_upper`: Upper bound of a 95% posterior interval of effect size
  from the model. Specific to the current study.

- `precision_ratio`: For the hierarchical model only, a model-based mean
  of the precision ratio. Specific to the current study.

- `precision_ratio_lower`: For the hierarchical model only, lower bound
  of a model-based 95% posterior interval of the precision ratio.
  Specific to the current study.

- `precision_ratio_upper`: For the hierarchical model only, upper bound
  of a model-based 95% posterior interval of the precision ratio.
  Specific to the current study.

- `mix_prop_*`: For the mixture model only, posterior mixture
  proportions of each of the mixture components. The last one is for the
  current study and the first ones are for the historical studies. The
  suffixes of these column names are the integer study indexes. Call
  `dplyr::distinct(hb_data(your_data), study, study_label)` to see which
  study labels correspond to these integer indexes.

## Details

The `hb_summary()` function post-processes the results from the model.
It estimates marginal means of the response, treatment effect, and other
quantities of interest.

## See also

Other summary:
[`hb_ess()`](https://wlandau.github.io/historicalborrow/reference/hb_ess.md)

## Examples

``` r
if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
data <- hb_sim_pool(n_continuous = 2)$data
data$group <- sprintf("group%s", data$group)
mcmc <- hb_mcmc_pool(
  data,
  n_chains = 1,
  n_adapt = 100,
  n_warmup = 50,
  n_iterations = 50
)
hb_summary(mcmc, data)
}
```
