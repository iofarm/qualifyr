# Class definition =============================================================

new_qf_dataset <- function(x) {
  stopifnot(is.list(x))
  stopifnot(purrr::every(x, is_qf_table))
  stopifnot(all(nchar(names(x)) > 0))

  # inherits from "list" so that purrr functions treat <qf_dataset> objects as
  # vectors rather than scalars. See ?vctrs::`vector-checks` for details.
  structure(x, class = c("qf_dataset", "list"))
}

validate_qf_dataset <- function(x) {
  purrr::walk(x, \(table)
    if (!is_qf_table(table))
      stop("A <qf_dataset> must only contain <qf_table> objects")
  )
  purrr::iwalk(x, \(table, name) tryCatch(
    validate_qf_table(table),
    error = \(e) stop(
      "Table ", name, " has invalid structure: \n",
      e$message
    )
  ))
  x
}

#' Create a new qualifyr data set from data frames
#'
#' @param ... Data frames to be included in the data set. If names are provided,
#'   they will be used as the table names; otherwise, the deparsed expressions
#'   will be used, which may or may not be a sensible default.
#'
#' @returns A new `<qf_dataset>` object containing the tables provided in `...`
#'   (after being converted to `<qf_table>` objects by `as_qf_table()`).
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
  validate_qf_dataset(new_qf_dataset(tables))
}

#' @rdname type_predicates
#' @export
is_qf_dataset <- function(x) {
  inherits(x, "qf_dataset")
}



# Methods ======================================================================

#' @rdname check_constraints
#' @export
check_constraints.qf_dataset <- function(x) {
  x |> purrr::map(check_constraints)
}

#' @noRd
#' @exportS3Method utils::str
str.qf_dataset <- function(object,
  nest.lev = 0,
  indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."),
  ...
) {
  cat("<qf_dataset> with ", length(object), " tables: \n", sep = "")

  table_names <- format(names(object))
  purrr::walk2(object, table_names, \(table, name) {
    cat(" $ ", name, ": ", sep = "")
    utils::str(table, nest.lev = nest.lev + 1, show.context = FALSE, ...)
  })

  invisible(object)
}

#' @noRd
#' @export
print.qf_dataset <- function(x, ...) {
  str(x)
}

#' Subsetting qualifyr data sets
#'
#' Accessing tables from qualifyr data sets works like subsetting bare lists,
#' except that certain context information is attached to the returned tables.
#' This is necessary for resolving and checking foreign keys that reference
#' other tables in the dataset.
#'
#' @param x A qualifyr data set
#' @param i Indices specifying tables - numeric for positional subsetting, or
#'   character for subsetting by name; must be length one for `[[` and `[[<-`
#' @param name The name of a table to subset, either as a character literal or
#'   raw symbol
#' @param value A qualifyr table to replace the subsetted table(s) with;
#'   or, for `[<-`, possibly a list of qualifyr tables or a qualifyr dataset
#'
#' @returns For `$` and `[[`, a qualifyr table. For `[`, a list of qualifyr
#'   tables. For `$<-`, `[[<-`, and `[<-`, a modified version of `x`.
#'
#' @details
#' Subsetted tables have `x` as their `context` attribute. The subset assignment
#' methods strip the `context` attribute from `value`.
#'
#' @name subsetting
NULL

#' @rdname subsetting
#' @export
`$.qf_dataset` <- function(x, name) {
  y <- NextMethod()
  if (!is.null(y)) attr(y, "context") <- x
  y
}
#' @rdname subsetting
#' @export
`[[.qf_dataset` <- function(x, i) {
  y <- NextMethod()
  if (!is.null(y)) attr(y, "context") <- x
  y
}
#' @rdname subsetting
#' @export
`[.qf_dataset` <- function(x, i) {
  y <- NextMethod()
  purrr::modify(y, \(table) {
    if (!is.null(table)) attr(table, "context") <- x
    table
  })
}
#' @rdname subsetting
#' @export
`$<-.qf_dataset` <- function(x, name, value) {
  value <- as_qf_table(value)
  attr(value, "context") <- NULL
  validate_qf_dataset(NextMethod())
}
#' @rdname subsetting
#' @export
`[[<-.qf_dataset` <- function(x, i, value) {
  value <- as_qf_table(value)
  attr(value, "context") <- NULL
  validate_qf_dataset(NextMethod())
}
#' @rdname subsetting
#' @export
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
  validate_qf_dataset(NextMethod())
}
