# Standardize data

Standardize a tidy input dataset.

## Usage

``` r
hb_data(
  data,
  response,
  study,
  study_reference,
  group,
  group_reference,
  patient,
  covariates
)
```

## Arguments

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

- covariates:

  Character vector of column names in `data` with the columns with
  baseline covariates. These can be continuous, categorical, or binary.
  Regardless, `historicalborrow` derives the appropriate model matrix.

## Value

A standardized tidy data frame with one row per patient and the
following columns:

- `response`: continuous response/outcome variable. (Should be change
  from baseline of an outcome of interest.)

- `study_label`: human-readable label of the study.

- `study`: integer study index with the max index equal to the current
  study (at `study_reference`).

- `group_label`: human-readable group label (e.g. treatment arm name).

- `group`: integer group index with an index of 1 equal to the control
  group (at `group_reference`).

- `patient_label`: original patient ID.

- `patient`: integer patient index.

- `covariate_*`: baseline covariate columns.

## Details

Users do not normally need to call this function. It mainly serves
exposes the indexing behavior of studies and group levels to aid in
interpreting summary tables.

## Examples

``` r
data <- hb_sim_independent(n_continuous = 1, n_study = 2)$data
data <- dplyr::select(
  data,
  study,
  group,
  patient,
  response,
  tidyselect::everything()
)
colnames(data) <- c("trial", "arm", "subject", "change", "cov1", "cov2")
data$trial <- paste0("trial", data$trial)
data$arm <- paste0("arm", data$arm)
hb_data(
  data = data,
  response = "change",
  study = "trial",
  study_reference = "trial1",
  group = "arm",
  group_reference = "arm1",
  patient = "subject",
  covariates = c("cov1", "cov2")
)
#> # A tibble: 400 × 9
#>    response study_label group_label patient_label study group patient
#>       <dbl> <chr>       <chr>               <int> <int> <int>   <int>
#>  1   1.42   trial2      arm1                  101     1     1     101
#>  2   1.79   trial2      arm1                  102     1     1     102
#>  3   0.348  trial2      arm1                  103     1     1     103
#>  4  -0.631  trial2      arm1                  104     1     1     104
#>  5   0.0563 trial2      arm1                  105     1     1     105
#>  6  -0.356  trial2      arm1                  106     1     1     106
#>  7   1.08   trial2      arm1                  107     1     1     107
#>  8  -0.984  trial2      arm1                  108     1     1     108
#>  9  -2.87   trial2      arm1                  109     1     1     109
#> 10   0.707  trial2      arm1                  110     1     1     110
#> # ℹ 390 more rows
#> # ℹ 2 more variables: covariate_cov1 <dbl>, covariate_cov2 <dbl>
```
