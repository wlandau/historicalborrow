true <- function(
  value = NULL,
  ...,
  message = NULL,
  envir = parent.frame()
) {
  expr <- match.call(expand.dots = FALSE)$...
  if (!length(expr)) {
    expr <- list(quote(.))
  }
  conditions <- lapply(
    expr,
    function(expr) all(eval(expr, envir = list(. = value), enclos = envir))
  )
  if (!all(unlist(conditions))) {
    chr_expr <- lapply(expr, function(x) sprintf("all(%s)", deparse(x)))
    chr_expr <- paste(unlist(chr_expr), collapse = " && ")
    chr_value <- deparse(substitute(value))
    out <- sprintf("%s is not true on . = %s", chr_expr, chr_value)
    hb_error(message %|||% out)
  }
}

hb_warn_identifiable <- function(response, x_alpha, x_delta, x_beta) {
  x <- cbind(x_alpha, x_delta, x_beta)
  recommendation <- paste(
    "Please check your baseline covariates. For categorical",
    "covariates with many levels, you may need to pool some of those",
    "levels so the fixed effects can be identified.",
    "You may need to do this within each individual study",
    "because covariates are incorporated into the model",
    "in a study-specific manner."
  )
  if (!is_full_rank(x)) {
    message <- paste("full model matrix is not full rank.", recommendation)
    hb_warn(message = message)
    return()
  }
  x <- x[!is.na(response),, drop = FALSE] # nolint
  if (!is_full_rank(x)) {
    message <- paste(
      "full model matrix is not full rank after removing rows",
      "corresponding to missing values in the response variable.",
      recommendation
    )
    hb_warn(message = message)
  }
}

hb_error <- function(message) {
  rlang::abort(message = message, class = "hb_error", .frame = emptyenv())
}

hb_warn <- function(message) {
  rlang::warn(message = message, class = "hb_warn")
}
