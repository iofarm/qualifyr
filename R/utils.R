#' @noRd
#'
#' @description
#' Convenience wrapper for `tidyselect::eval_select()` that (1) returns column
#' names instead of named positions and (2) defaults to `allow_rename = FALSE`.
#'
#' @inheritParams tidyselect::eval_select
#' @inheritDotParams tidyselect::eval_select
select_names <- function(expr, data, allow_rename = FALSE, ...) {
  names(tidyselect::eval_select(expr, data, allow_rename = allow_rename, ...))
}

#' Get data mask from caller environment
#'
#' When called from a function called by an expression under tidy evaluation,
#' extracts the data mask used for tidy evaluation.
#'
#' @noRd
caller_mask <- function() {
  mask <- rlang::eval_bare(get0(".data"), rlang::caller_env(n = 2))
  if (is.null(mask) || inherits(mask, "rlang_fake_data_pronoun"))
    stop("Caller environment is not a data-masking context")
  mask
}
