new_qf_dataset <- function(x) {
  stopifnot(is.list(x))
  stopifnot(all(purrr::map_lgl(x, is_qf_dataframe)))
  stopifnot(all(nchar(names(x)) > 0))

  structure(x, class = "qf_dataset")
}

qf_dataset <- function(...) {
  dataframe_exprs <- rlang::enexprs(...) |> purrr::map_chr(deparse)
  dataframe_list <- list(...) |> purrr::modify(as_qf_dataframe)
  names(dataframe_list) <- ifelse(
    nchar(names(dataframe_list) > 0),
    names(dataframe_list), dataframe_exprs
  )
  new_qf_dataset(dataframe_list)
}

is_qf_dataset <- function(x) {
  inherits(x, "qf_dataset")
}


#' @noRd
#' @exportS3Method constraints qf_dataset
constraints.qf_dataset <- function(x, table, ...) {
  table_chr <- rlang::as_string(rlang::ensym(table))
  constraints(x[[table_chr]])
}

#' @noRd
#' @exportS3Method "constraints<-" qf_dataset
`constraints<-.qf_dataset` <- function(x, table, ..., value) {
  table_chr <- rlang::as_string(rlang::ensym(table))
  if (!rlang::is_list(value)) stop("'value' must be a list")
  constraints(x[[table_chr]]) <- value |> purrr::modify(\(cstr) {
    if (is_qf_constraint(cstr)) {
      cstr
    } else if (rlang::is_function(cstr) && inherits(cstr, "cstr_builder")) {
      cstr(x, x[[table_chr]])
    } else stop("Elements of 'value' must be either constraint objects or
      constraint builders returned by cstr_* functions")
  })
  x
}

check_constraints <- function(x, ...) {
  UseMethod("check_constraints")
}

#' @noRd
#' @exportS3Method check_constraints qf_dataset
check_constraints.qf_dataset <- function(x, ...) {
  x |> purrr::map(check_constraints, x)
}

#' @noRd
#' @exportS3Method base::as.list qf_dataset
as.list.qf_dataset <- function(x, ...) {
  unclass(x)
}
