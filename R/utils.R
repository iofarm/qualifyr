# Element selection ============================================================

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



# Pretty printing ==============================================================

cat0 <- function(..., file = "", sep = "", fill = FALSE, labels = NULL,
  append = FALSE) {
  cat(..., file = file, sep = sep, fill = fill, label = labels, append = append)
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

#' Format a vector of indices compactly
#'
#' e.g., c(1, 3, 4, 5, 7, 8) => "1, 3-5, 7-8"
#'
#' @param x An integer vector
#' @param max_len The maximum length of the output, in characters
#'
#' @returns Character
#'
#' @noRd
format_indices <- function(x, max_len = Inf) {
  stopifnot(is.integer(x))
  x <- sort(x)

  best_copout <- sprintf("integer(%d)", length(x)) |> substr(1, max_len)
  runs <- list()
  for (i in 1:length(x)) {
    if (length(runs) == 0)
      runs <- list(x[[i]])
    else if (x[[i]] == last(last(runs)) + 1)
      last(runs) <- c(last(runs), x[[i]])
    else
      runs <- c(runs, list(x[[i]]))

    formatted <- runs |>
      purrr::map_chr(\(v) {
        if (length(v) == 1)
          paste(v)
        else
          paste(first(v), last(v), sep = "-")
      }) |>
      paste(collapse = ", ")

    copout <- sprintf("%s... (%d more)", formatted, length(x) - i)
    if (nchar(copout) <= max_len) best_copout <- copout

    if (nchar(formatted) > max_len)
      return(best_copout)
    else if (i == length(x))
      return(formatted)
  }
}



# Miscellaneous ================================================================

check_arg_type <- function(arg, type, predicate = \(x) inherits(x, type)) {
  arg_name <- rlang::as_string(rlang::ensym(arg))
  if (!(predicate)(arg))
    stop(sprintf("'%s' must be a %s, not %s", arg_name, type, typeof(arg)))
}

first <- function(x) {
  x[[1]]
}
`first<-` <- function(x, value) {
  x[[1]] <- value
  x
}
last <- function(x) {
  x[[length(x)]]
}
`last<-` <- function(x, value) {
  x[[length(x)]] <- value
  x
}



# Debugging ====================================================================

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
