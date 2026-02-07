#' @describeIn cstr_ Requires that each row has a unique set of values for the
#'   key columns.
#' @order 1
#' @export
cstr_unique_key <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  new_constraint_specifier({
    new_qf_constraint(
      list(cols = select_names(cols_quo, .table)),
      "cstr_unique_key"
    )
  })
}

#' @noRd
#' @export
validate_qf_constraint.cstr_unique_key <- function(x, table) {
  NextMethod()
}

#' @noRd
#' @export
check_constraint_strict.cstr_unique_key <- function(constraint, table) {
  key_cols <- table[constraint$cols]

  is_duplicated <- duplicated(key_cols) | duplicated(key_cols, fromLast = TRUE)

  result <- !is_duplicated
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
