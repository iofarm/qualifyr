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



# Exception lists ==============================================================

new_qf_exception_list <- function(x) {
  stopifnot(is.list(x))
  stopifnot(purrr::every(x, is_qf_exception))
  structure(x, class = c("qf_exception_list", "list"))
}

is_qf_exception_list <- function(x) {
  inherits(x, "qf_exception_list")
}

as_qf_exception_list <- function(x) {
  UseMethod("as_qf_exception_list")
}

#' @noRd
#' @export
as_qf_exception_list.qf_exception_list <- function(x) {
  x
}
#' @noRd
#' @export
as_qf_exception_list.list <- function(x) {
  purrr::walk(x, \(cstr) {
    if (!is_qf_exception(cstr))
      stop("<qf_exception_list> cannot include a ", typeof(x))
  })
  new_qf_exception_list(x)
}
#' @noRd
#' @export
as_qf_exception_list.qf_exception <- function(x) {
  new_qf_exception_list(list(x))
}

#' @noRd
#' @export
`|.qf_exception_list` <- function(e1, e2) {
  new_qf_exception_list(c(
    as_qf_exception_list(e1),
    as_qf_exception_list(e2))
  )
}
#' @noRd
#' @export
`|.qf_exception` <- `|.qf_exception_list`



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
