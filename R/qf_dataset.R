# Class definition =============================================================

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

# Methods ======================================================================

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

#' @noRd
#' @exportS3Method base::`$` qf_dataset
`$.qf_dataset` <- function(x, name) {
  y <- NextMethod()
  attr(y, "context") <- as.list(x)
  y
}
#' @noRd
#' @exportS3Method base::`[[` qf_dataset
`[[.qf_dataset` <- function(x, i) {
  y <- NextMethod()
  attr(y, "context") <- as.list(x)
  y
}
#' @noRd
#' @exportS3Method base::`[` qf_dataset
`[.qf_dataset` <- function(x, i) {
  y <- NextMethod()
  purrr::modify(y, \(table) {
    attr(table, "context") <- as.list(x)
    table
  })
}
#' @noRd
#' @exportS3Method base::`$` qf_dataset
`$<-.qf_dataset` <- function(x, name, value) {
  value <- as_qf_table(value)
  attr(value, "context") <- NULL
  NextMethod()
}

#' @noRd
#' @exportS3Method base::`[[` qf_dataset
`[[<-.qf_dataset` <- function(x, i, value) {
  value <- as_qf_table(value)
  attr(value, "context") <- NULL
  NextMethod()
}

#' @noRd
#' @exportS3Method base::`[` qf_dataset
`[<-.qf_dataset` <- function(x, i, value) {
  if (is_qf_table(value)) {
    attr(value, "context") <- NULL
  } else if (is.list(value)) {
    value <- purrr::modify(value, \(table) {
      table <- as_qf_table(table)
      attr(table, "context") <- NULL
      table
    })
  }
  NextMethod()
}

#' @noRd
#' @exportS3Method utils::str qf_dataset
str.qf_dataset <- function(object, ...) {
  cat("<qf_dataset>\n")
  cat("qualifyr data set with ", length(object), " tables: \n", sep = "")
  purrr::iwalk(as.list(object), \(table, name) {
    cat(" $ ", name, ": ", sep = "")
    utils::str(table)
  })
}
