#' Check constraints on qualifyr data set or table
#'
#' Checks that a data set or table satisfies the constraints set on it.
#'
#' @param x A `<qf_dataset>` or `<qf_table>` object
#' @param dataset If `x` is a table with foreign key constraints, the
#'   data set to reference foreign keys against. Can be `NULL` if `x` has no
#'   foreign keys.
#' @param ... Further arguments passed to methods
#'
#' @rdname check_constraints
check_constraints <- function(x, ...) {
  UseMethod("check_constraints")
}

#' Get or set constraints of a qualifyr table
#'
#' @param x A qualifyr data set or table to set constraints on
#' @param table <[`tidy-select`][constraints]> If `x` is a
#'   data set, the table(s) to set constraints on, either as a character string
#'   or bare symbol
#' @param dataset If `x` is a data frame, the data set used to reference
#'   foreign keys against. Only needed if `value` contains a foreign key.
#' @param ... Additional arguments passed to methods
#' @param value A list of constraints (or constraint builder functions)
#'   created with `cstr_*()` functions.
#'
#' @returns For `constraints`, the list of `<qf_constraint>` objects associated
#'   with the table. For `constraints<-`, an updated version of `x` with
#'   constraints set.
#'
#' @rdname constraints
#' @export
constraints <- function(x, ...) {
  UseMethod("constraints")
}
#' @rdname constraints
#' @export
`constraints<-` <- function(x, ..., value) {
  UseMethod("constraints<-")
}

#' @name type-predicates
#' @rdname type-predicates
#'
#' @title Test for qualifyr objects
#'
#' @param x The object to test the type of
#'
#' @returns `TRUE` if `x` inherits from the specified class; `FALSE` otherwise.
NULL
