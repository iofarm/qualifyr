#' @rdname cstr_
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
#' @exportS3Method validate_constraint cstr_unique_key
validate_constraint.cstr_unique_key <- function(constraint, table) {
  NextMethod()
}

#' @noRd
#' @exportS3Method check_constraint cstr_unique_key
check_constraint.cstr_unique_key <- function(constraint, table) {
  key_cols <- table[constraint$cols]

  is_duplicated <- duplicated(key_cols) | duplicated(key_cols, fromLast = TRUE)

  result <- !is_duplicated
  attr(result, "constraint") <- constraint
  result
}

