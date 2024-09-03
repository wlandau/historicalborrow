#' historicalborrow: Bayesian historical borrowing models for clinical studies.
#' @description Bayesian historical borrowing models for clinical studies.
#' @name historicalborrow-package
#' @family help
#' @importFrom dplyr across arrange bind_cols bind_rows distinct filter group_by
#'   left_join mutate n rename select summarize ungroup
#' @importFrom ggplot2 aes geom_errorbar geom_point ggplot
#'   position_dodge theme_gray xlab ylab
#' @importFrom Matrix bdiag
#' @importFrom posterior as_draws_df mcse_mean mcse_sd mcse_quantile
#' @importFrom rjags coda.samples jags.model
#' @importFrom rlang abort
#' @importFrom stats as.formula model.matrix qnorm quantile rnorm rt runif
#'   sd update var
#' @importFrom tibble as_tibble
#' @importFrom tidyr expand_grid pivot_longer pivot_wider
#' @importFrom tidyselect everything starts_with
#' @importFrom utils capture.output globalVariables
NULL

utils::globalVariables(
  c(
    ".",
    "..density..",
    "Group",
    "group",
    "group_label",
    "Model",
    "data_mean",
    "data_sd",
    "response",
    "response_mean",
    "response_lower",
    "response_upper",
    "study",
    "study_index",
    "study_label",
    "tau",
    "value",
    "value_percent"
  )
)
