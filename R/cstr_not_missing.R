#' @describeIn cstr_ Requires that the specified columns do not contain missing
#'   values.
#' @order 4
#' @export
cstr_not_missing <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  new_constraint_specifier({
    new_qf_constraint(
      list(cols = select_names(cols_quo, .table)),
      "cstr_not_missing"
    )
  })
}

#' @noRd
#' @export
validate_qf_constraint.cstr_not_missing <- function(x, table) {
  NextMethod()
}

#' @noRd
#' @export
check_constraint_strict.cstr_not_missing <- function(constraint, table) {
  key_cols <- table[constraint$cols]
  result <- !apply(key_cols, 1, anyNA)
  attr(result, "constraint") <- constraint
  result
}

#' @rdname pick_constraint
#' @order 4
#' @export
not_missing <- function(table, cols = NULL) {
  constraint(table, {{ cols }}, "cstr_not_missing")
}
#' @rdname pick_constraint
#' @order 14
#' @export
`not_missing<-` <- function(table, cols = NULL, value) {
  constraint(table, {{ cols }}, "cstr_not_missing") <- value
  table
}
