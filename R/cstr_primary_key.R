#' @rdname cstr_
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
#' @exportS3Method validate_constraint cstr_primary_key
validate_constraint.cstr_primary_key <- function(constraint, table) {
  NextMethod()
}

#' @noRd
#' @exportS3Method check_constraint cstr_primary_key
check_constraint.cstr_primary_key <- function(constraint, table) {
  key_cols <- table[constraint$cols]

  is_na <- key_cols |> purrr::map(is.na) |> purrr::reduce(`|`)

  result <- NextMethod() & (!is_na)
  attr(result, "constraint") <- constraint
  result
}
