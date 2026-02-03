# Class definition =============================================================

new_qf_dataset <- function(x) {
  stopifnot(is.list(x))
  stopifnot(purrr::every(x, is_qf_table))
  stopifnot(all(nchar(names(x)) > 0))

  # inherits from "list" so that purrr functions treat <qf_dataset> objects
  #   as vectors rather than scalars. See ?vctrs::`vector-checks` for details.
  structure(x, class = c("qf_dataset", "list"))
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
  names_implicit <- rlang::enexprs(...) |> purrr::map_chr(deparse)
  tables <- list(...) |> purrr::modify(as_qf_table)
  names_explicit <-
    if (is.null(names(tables))) rep("", length(tables))
    else names(tables)
  names(tables) <- ifelse(
    nchar(names_explicit) > 0 & !is.na(names_explicit),
    names_explicit, names_implicit
  )
  new_qf_dataset(tables)
}

#' @rdname type-predicates
#' @export
is_qf_dataset <- function(x) {
  inherits(x, "qf_dataset")
}

# Methods ======================================================================

#' @rdname check_constraints
#' @exportS3Method check_constraints qf_dataset
check_constraints.qf_dataset <- function(x) {
  x |> purrr::map(check_constraints)
}

# #' @noRd
# #' @exportS3Method base::as.list qf_dataset
# as.list.qf_dataset <- function(x, ...) {
#   unclass(x)
# }

#' @noRd
#' @exportS3Method base::`$` qf_dataset
`$.qf_dataset` <- function(x, name) {
  y <- NextMethod()
  if (!is.null(y)) attr(y, "context") <- x
  y
}
#' @noRd
#' @exportS3Method base::`[[` qf_dataset
`[[.qf_dataset` <- function(x, i) {
  y <- NextMethod()
  if (!is.null(y)) attr(y, "context") <- x
  y
}
#' @noRd
#' @exportS3Method base::`[` qf_dataset
`[.qf_dataset` <- function(x, i) {
  y <- NextMethod()
  purrr::modify(y, \(table) {
    if (!is.null(table)) attr(table, "context") <- x
    table
  })
}
#' @noRd
#' @exportS3Method base::`$<-` qf_dataset
`$<-.qf_dataset` <- function(x, name, value) {
  value <- as_qf_table(value)
  attr(value, "context") <- NULL
  NextMethod()
}

#' @noRd
#' @exportS3Method base::`[[<-` qf_dataset
`[[<-.qf_dataset` <- function(x, i, value) {
  value <- as_qf_table(value)
  attr(value, "context") <- NULL
  NextMethod()
}

#' @noRd
#' @exportS3Method base::`[<-` qf_dataset
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
  purrr::iwalk(unclass(object), \(table, name) {
    cat(" $ ", name, ": ", sep = "")
    utils::str(table)
  })
}
