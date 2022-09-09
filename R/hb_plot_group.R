#' @title Plot the groups of a borrowing model
#'   and its benchmark models.
#' @export
#' @family plot
#' @description Plot the groups against one another
#'   for a borrowing model (hierarchical or mixture)
#'   and the independent and pooled benchmark models.
#' @return A `ggplot` object
#' @inheritParams hb_plot_borrow
#' @examples
#' data <- hb_sim_independent(n_continuous = 2)$data
#' mcmc_borrow <- hb_mcmc_hierarchical(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 100,
#'   n_iterations = 200
#' )
#' mcmc_pool <- hb_mcmc_pool(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 200,
#'   n_iterations = 200
#' )
#' mcmc_independent <- hb_mcmc_independent(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 200,
#'   n_iterations = 200
#' )
#' borrow <- hb_summary(mcmc_borrow, data)
#' pool <- hb_summary(mcmc_pool, data)
#' independent <- hb_summary(mcmc_independent, data)
#' hb_plot_group(
#'   borrow = borrow,
#'   pool = pool,
#'   independent = independent
#' )
hb_plot_group <- function(
  borrow,
  pool,
  independent,
  outcome = c("response", "diff")
) {
  outcome <- match.arg(outcome)
  true(is.data.frame(borrow))
  true(is.data.frame(pool))
  true(is.data.frame(independent))
  true(nrow(borrow) == nrow(pool))
  true(nrow(borrow) == nrow(independent))
  true("group" %in% colnames(borrow))
  true(all(borrow$group == pool$group))
  true(all(borrow$group == independent$group))
  for (name in paste0(outcome, c("_mean", "_lower", "_upper"))) {
    true(name %in% colnames(borrow))
    true(name %in% colnames(pool))
    true(name %in% colnames(independent))
  }
  borrow <- dplyr::select(
    borrow,
    group,
    group_label,
    tidyselect::starts_with(outcome)
  )
  pool <- dplyr::select(
    pool,
    group,
    group_label,
    tidyselect::starts_with(outcome)
  )
  independent <- dplyr::select(
    independent,
    group,
    group_label,
    tidyselect::starts_with(outcome)
  )
  out <- dplyr::bind_rows(
    `1-independent` = independent,
    `2-borrow` = borrow,
    `3-pool` = pool,
    .id = "Model"
  )
  out$Group <- as.character(out$group_label)
  out <- out[!is.na(out[[paste0(outcome, "_mean")]]),, drop = FALSE] # nolint
  ggplot2::ggplot(out) +
    ggplot2::geom_point(
      ggplot2::aes_string(
        x = "Model",
        y = paste0(outcome, "_mean"),
        color = "Group"
      ),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes_string(
        x = "Model",
        ymin = paste0(outcome, "_lower"),
        ymax = paste0(outcome, "_upper"),
        color = "Group"
      ),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::ylab("Posterior response") +
    ggplot2::theme_gray(20)
}
