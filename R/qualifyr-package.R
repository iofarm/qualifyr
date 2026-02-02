#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' @name type-predicates
#' @rdname type-predicates
#'
#' @title Test for qualifyr objects
#'
#' @param x The object to test the type of
#'
#' @returns `TRUE` if `x` inherits from the specified class; `FALSE` otherwise.
NULL

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
