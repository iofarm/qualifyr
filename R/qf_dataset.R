new_qf_dataset <- function(x) {
  stopifnot(is.list(x))
  stopifnot(all(purrr::map_lgl(x, is_qf_table)))
  stopifnot(all(nchar(names(x)) > 0))

  structure(x, class = "qf_dataset")
}

#' Create a new qualifyr data set from data frames
#'
#' @param ... Data frames to be included in the data set. If names are provided,
#'   they will be used as the table names; otherwise, the deparsed expressions
#'   will be used, which may or may not be a sensible default.
#'
#' @returns A new <qf_dataset> object containing the tables provided in `...`
#'   (after being converted to <qf_table> objects by `as_qf_table()`).
#'
#' @export
qf_dataset <- function(...) {
  dataframe_exprs <- rlang::enexprs(...) |> purrr::map_chr(deparse)
  dataframe_list <- list(...) |> purrr::modify(as_qf_table)
  names(dataframe_list) <- ifelse(
    nchar(names(dataframe_list)) > 0,
    names(dataframe_list), dataframe_exprs
  )
  new_qf_dataset(dataframe_list)
}

#' @rdname type-predicates
#' @export
is_qf_dataset <- function(x) {
  inherits(x, "qf_dataset")
}


#' @rdname constraints
#' @exportS3Method constraints qf_dataset
constraints.qf_dataset <- function(x, table, ...) {
  table_chr <- rlang::as_string(rlang::ensym(table))
  constraints(x[[table_chr]])
}

#' @rdname constraints
#' @exportS3Method "constraints<-" qf_dataset
`constraints<-.qf_dataset` <- function(x, table, ..., value) {
  table_chrs <- select_names(rlang::enquo(table), as.list(x))
  purrr::walk(table_chrs, \(table_chr) {
    constraints(x[[table_chr]], dataset = x) <<- value
  })
  x
}

#' @rdname check_constraints
#' @exportS3Method check_constraints qf_dataset
check_constraints.qf_dataset <- function(x, ...) {
  x |> purrr::map(check_constraints, x)
}

#' @noRd
#' @exportS3Method base::as.list qf_dataset
as.list.qf_dataset <- function(x, ...) {
  unclass(x)
}
