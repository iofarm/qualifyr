#' @describeIn cstr_ Requires that the specified columns do not contain missing
#'   values.
#' @order 4
#'
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
#' @exportS3Method validate_constraint cstr_not_missing
validate_constraint.cstr_not_missing <- function(constraint, table) {
  NextMethod()
}

#' @noRd
#' @exportS3Method check_constraint_strict cstr_not_missing
check_constraint_strict.cstr_not_missing <- function(constraint, table) {
  key_cols <- table[constraint$cols]
  result <- !apply(key_cols, 1, anyNA)
  attr(result, "constraint") <- constraint
  result
}

#' @rdname pick_constraint
#' @order 1
#' @export
unique_key <- function(table, cols = NULL) {
  constraint(table, {{ cols }}, "cstr_unique_key")
}
#' @rdname pick_constraint
#' @order 11
#' @export
`unique_key<-` <- function(table, cols = NULL, value) {
  constraint(table, {{ cols }}, "cstr_unique_key") <- value
  table
}
