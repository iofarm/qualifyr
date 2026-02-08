#' Select columns as names using tidyselect syntax
#'
#' Convenience wrapper for `tidyselect::eval_select()` that (1) returns column
#' names instead of named positions and (2) defaults to `allow_rename = FALSE`.
#'
#' @inheritParams tidyselect::eval_select
#' @inheritDotParams tidyselect::eval_select
#'
#' @returns A character vector of column names
#'
#' @noRd
select_names <- function(expr, data, allow_rename = FALSE, ...) {
  names(tidyselect::eval_select(expr, data, allow_rename = allow_rename, ...))
}

#' Resolve a reference to a qualifyr table
#'
#' Retrieves a qualifyr table object given a reference, as a string, to that
#' table.
#'
#' @param table The table the reference is relative to. If `reference` is not
#' `".self"`, then `table` must have the `context` attribute set
#' @param reference A length-1 character naming another table in the dataset, or
#' `".self"` to refer to `table`.
#'
#' @returns A `<qf_table>`
#'
#' @noRd
resolve_table_reference <- function(table, reference) {
  stopifnot(is_qf_table(table))
  stopifnot(is.character(reference) && length(reference) == 1)
  if (reference == ".self") {
    table
  } else {
    # even though this is not an exported function, informative error messages
    # are given here so they do not need to be repeated everywhere that uses it
    if (!is_qf_dataset(attr(table, "context")))
      stop("Reference points to a separate table, but no information on other ",
        "tables in the dataset was found")
    if (!(reference %in% names(attr(table, "context"))))
      stop("Reference points to a non-existent table (", reference, ")")
    attr(table, "context")[[reference]]
  }
}

#' Format a class name
#'
#' Formats the class of an object in tidyverse style, e.g., `<class_name>`.
#'
#' @param object An object
#'
#' @returns A length-1 character; the most derived class of `object` wrapped in
#' angle brackets.
#'
#' @noRd
pretty_class <- function(object) {
  paste0("<", class(object)[[1]], ">")
}

#' Get the caller environment's data mask
#'
#' When called from a function called by an expression under tidy evaluation,
#' extracts the data mask used for tidy evaluation.
#'
#' @returns Specifically, the environment referred to by `.data` in the tidy
#' evaluation environment of the caller
#'
#' @noRd
caller_mask <- function() {
  mask <- rlang::eval_bare(get0(".data"), rlang::caller_env(n = 2))
  if (is.null(mask) || inherits(mask, "rlang_fake_data_pronoun"))
    stop("Caller environment is not a data-masking context")
  mask
}

#' Check if the function currently being executed was called by RStudio
#'
#' Useful for debugging functions that might be called by RStudio internals
#'
#' @returns `TRUE` if a function with a name matching `.rs.*` is in the call
#' stack; `FALSE` otherwise.
#'
#' @examples
#' # Invoke the browser unless the current function was called by RStudio
#' if (!called_by_rstudio()) browser()
#'
#' @noRd
called_by_rstudio <- function() {
  any(vapply(rlang::trace_back()$call, \(expr) {
    startsWith(as.character(expr[[1]]), ".rs.")
  }, logical(1)))
}
