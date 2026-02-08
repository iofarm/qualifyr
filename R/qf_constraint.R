# Class definition =============================================================

new_qf_constraint <- function(x, subclass) {
  stopifnot(is.list(x))
  stopifnot(is.character(subclass))

  structure(x, class = c(subclass, "qf_constraint"))
}

validate_qf_constraint <- function(x, table) {
  UseMethod("validate_qf_constraint")
}

#' @noRd
#' @export
validate_qf_constraint.qf_constraint <- function(x, table) {
  if (!setequal(x$cols, unique(x$cols)))
    stop(pretty_class(x), " includes duplicate columns")
  if (!all(x$cols %in% colnames(table)))
    stop(pretty_class(x), " includes non-existent columns: ",
      paste(setdiff(x$cols, colnames(table)), collapse = ", "))
  x
}

#' @rdname type_predicates
#' @export
is_qf_constraint <- function(x) {
  inherits(x, "qf_constraint")
}

as_qf_constraint <- function(x, ...) {
  UseMethod("as_qf_constraint")
}

#' @noRd
#' @export
as_qf_constraint.qf_constraint <- function(x, ...) {
  x
}

#' @noRd
#' @export
as_qf_constraint.qf_constraint_specifier <- function(x, table, ...) {
  (x)(table)
}



# Constraint specification =====================================================

#' Create a constraint specifier
#'
#' Constraint declarations (`cstr_*()` functions) are meant to be  called in the
#' RHS of the replacement-form function `constraints<-`. Because replacement-
#' form functions eagerly evaluate the RHS, we cannot use typical rlang-style
#' tidy evaluation to enrich the environment of the RHS expressions with
#' dataset and table information. Instead, the `cstr_*()` functions return
#' closures that enclose their tidy-select specifications and return  constraint
#' objects when given context information.
#'
#' @param code A raw expression to be used as the body of the constraint
#'   specifier function
#'
#' @returns A new closure of class `<qf_constraint_specifier>` that takes one
#'   argument (`.table`) and returns a `<qf_constraint>` object.
#'
#' @noRd
new_constraint_specifier <- function(code) {
  structure(
    rlang::new_function(
      alist(.table = ),
      body = rlang::enexpr(code),
      env = rlang::caller_env()
    ),
    class = c("qf_constraint_specifier", "qf_constraint")
  )
}

#' Parse reference specifiers for foreign keys
#'
#' This function implements and extension to the tidyselect domain-specific
#' language that enables specifying a table and columns in a single expression.
#'
#' @param expr A defused expression specifying a table and columns according to
#'   the tidyselect extension described in 'details'
#'
#' @returns A list with two elements, each a tidyselect expression: `$table`,
#'   the expression specifying the table, and `$cols`, the expression specifying
#'   the columns.
#'
#' @details To select columns from a specific table, use either:
#'
#'   * `table$column`, where `table` and `column` are tidyselect specifiers for
#'   the table and columns, respectively; or
#'   * `table[column1, column2, ...]` where `table` specifies the table and
#'   `column1, column2, ...` specify columns from `table`
#'
#'   If `expr` is not a call to `$` or `[`, then it will be returned as `$table`
#'   while `$col` is `NULL`.
#'
#' @noRd
parse_reference_specifier <- function(expr) {
  is_subset_expr <- rlang::is_call(expr) &&
    rlang::as_string(expr[[1]]) %in% c("$", "[")
  if (is_subset_expr) list(
    table = expr[[2]],
    cols =
      if (expr[[1]] == "$")
        expr[[3]]
    else if (expr[[1]] == "[")
      rlang::call2("c", !!!as.list(expr[-(1:2), drop = FALSE]))
    else stopifnot(FALSE)
  ) else list(
    table = expr,
    cols = NULL
  )
}

# declare data pronouns to avoid R CMD CHECK notes
utils::globalVariables(c(".table"))

# Documentation topic for cstr_*() functions:

#' Constraints for relational data structures
#'
#' Functions to specify constraints for qualifyr tables. The return values
#' should be stored in a list and passed to `constraints<-`.
#'
#' @param cols <[`tidy-select`][args_tidy_select]> The columns to set
#'   constraints on
#' @param reference <[`tidy-select`][args_tidy_select]> For
#'   `cstr_foreign_key()`, the table and columns to reference. If the key
#'   columns and reference columns have the same names, you can specify just the
#'   table. Otherwise, specify the columns using `table$columns` or
#'   `table[columns]` where `table` and `column`/`columns` are each a
#'   tidy-select specification. The special value `.self` (as a bare symbol)
#'   refers to the current table.
#'
#' @returns A `<qf_constraint_specifier>` object.
#'
#' @details The return value, a `<qf_constraint_specifier>` object, is
#'   technically a closure which takes a qualifyr table and returns a
#'   `<qf_constraint>` object. This is a somewhat unfortunate implementation
#'   detail, but it allows using `cstr_*()` functions in the RHS of ``
#'   `constraints<-`() `` without needing to include the table as an argument,
#'   making front-end syntex a little clearer.
#'
#' @name cstr_
NULL



# Generics =====================================================================

#' Check that a constraint is satisfied
#'
#' @param constraint A `<qf_constraint>` object
#' @param table The table to which the constraint is applied
#'
#' @returns A list; see `check_constraints` for details.
#'
#' @noRd
check_constraint <- function(constraint, table) {
  satisfied_rows <- check_constraint_strict(constraint, table)
  excepted_rows <- attr(constraint, "exceptions") |>
    purrr::map(excepted_rows, table = table) |>
    purrr::reduce(`|`, .init = rep(FALSE, nrow(table)))
  handled_rows <- satisfied_rows | excepted_rows
  list(
    satisfied = all(satisfied_rows),
    handled = all(handled_rows),
    rows = list(
      satisfied = satisfied_rows,
      excepted = excepted_rows,
      handled = handled_rows
    )
  )
}

#' Check that a constraint is satisfied, ignoring exceptions
#'
#' @param constraint A `<qf_constraint>` object
#' @param table The table to which the constraint is applied
#'
#' @returns A vector of length `nrow(table)` indicating whether the constraint
#'   is satisfied for each row of the table.
#'
#' @noRd
check_constraint_strict <- function(constraint, table) {
  UseMethod("check_constraint_strict")
}



# Exception handling ===========================================================

#' Get or set the exceptions of a constraint
#'
#' @param x A constraint object
#' @param value A list of exceptions
#'
#' @returns  For `exceptions`, the list of `<qf_exception>` objects associated
#'   with the constraint For `exceptions<-`, an updated version of `x` with
#'   exceptions set.
#'
#' @export
exceptions <- function(x) {
  if (!is_qf_constraint(x))
    stop("'x' must be a <qf_constraint>, not ", typeof(x))

  attr(x, "exceptions")
}

#' @rdname exceptions
#' @export
`exceptions<-` <- function(x, value) {
  if (!is_qf_constraint(x))
    stop("'x' must be a <qf_constraint>, not ", typeof(x))
  exception_list <-
  if (is_qf_exception(value))
    list(value)
  else if (is.list(value))
    purrr::walk(value, \(expt) if (!is_qf_exception(expt)) stop("'value' must
      contain only <qf_exception> objects, not ", typeof(expt)))
  else stop("'value' must be a <qf_exception> or list of <qf_exception> objects,
    not ", typeof(value))

  attr(x, "exceptions") <- exception_list
  x
}
