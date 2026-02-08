# Class definition =============================================================

new_qf_exception <- function(conditions) {
  stopifnot(rlang::is_list(conditions))
  stopifnot(purrr::every(conditions, rlang::is_quosure))
  structure(conditions, class = "qf_exception")
}

#' @rdname type_predicates
#' @export
is_qf_exception <- function(x) {
  inherits(x, "qf_exception")
}



# Exception specification ======================================================

#' Define a constraint exception
#'
#' @param ... <[`data-masking`][rlang::args_data_masking]> Conditions under
#'   which to except a row. Each condition, when using the table as a data mask,
#'   should evaluate to a logical vector with length equal to the number of rows
#'   of the table. Rows for which all conditions are `TRUE` will be excepted.
#'
#' @returns A `<qf_exception>` object
#'
#' @export
except_where <- function(...) {
  new_qf_exception(rlang::enquos(...))
}



# Exception checking ===========================================================

#' Check which rows are excepted
#'
#' @param exception A `<qf_exception>` object
#' @param table The `<qf_table>` to check the exception against
#'
#' @returns A logical vector of length `nrow(table)` indicating whether each row
#'   is excepted (`TRUE`) or not (`FALSE`)
#'
#' @noRd
excepted_rows <- function(exception, table) {
  conditions <- purrr::map(exception, rlang::eval_tidy, data = table)
  purrr::walk(conditions, \(cond)
    if (!rlang::is_logical(cond, n = nrow(table)))
      stop("Each condition must evaluate to a logical vector of length ",
        nrow(table), ", not a ", typeof(cond), " of length ", length(cond))
  )
  purrr::reduce(conditions, `&`)
}
