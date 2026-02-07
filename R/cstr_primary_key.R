#' @describeIn cstr_ Requires that each row has a unique set of values for the
#'   key columns, and is not missing values in the key columns.
#' @order 2
#' @export
cstr_primary_key <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  new_constraint_specifier({
    new_qf_constraint(
      list(cols = select_names(cols_quo, .table)),
      c("cstr_primary_key", "cstr_unique_key")
    )
  })
}

#' @noRd
#' @export
validate_constraint.cstr_primary_key <- function(constraint, table) {
  NextMethod()
}

#' @noRd
#' @export
check_constraint_strict.cstr_primary_key <- function(constraint, table) {
  key_cols <- table[constraint$cols]

  is_na <- key_cols |> purrr::map(is.na) |> purrr::reduce(`|`)

  result <- NextMethod() & (!is_na)
  attr(result, "constraint") <- constraint
  result
}

#' @rdname pick_constraint
#' @order 2
#' @export
primary_key <- function(table, cols = NULL) {
  constraint(table, {{ cols }}, "cstr_primary_key")
}
#' @rdname pick_constraint
#' @order 12
#' @export
`primary_key<-` <- function(table, cols = NULL, value) {
  constraint(table, {{ cols }}, "cstr_primary_key") <- value
  table
}
