# Class definition =============================================================

new_qf_constraint <- function(x, subclass) {
  stopifnot(is.list(x))
  stopifnot(is.character(subclass))

  structure(x, class = c(subclass, "qf_constraint"))
}

validate_qf_constraint <- function(x, table, call = rlang::caller_env()) {
  UseMethod("validate_qf_constraint")
}

#' @noRd
#' @export
validate_qf_constraint.qf_constraint <- function(x, table, call) {
  if (!setequal(x$cols, unique(x$cols)))
    abort("constraint includes duplicate columns", call = call, cstr = x)
  if (!all(x$cols %in% colnames(table)))
    abort(
      sprintf("constraint includes non-existent columns: %s",
        paste(setdiff(x$cols, colnames(table)), collapse = ", ")),
      call = call, cstr = x
    )

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
  y <- (x)(table)
  if (is_qf_constraint_list(y))
    stop("'x' specifies multiple constraints and cannot be coerced to a a",
      "single constraint")
  y
}



# Constraint lists =============================================================

new_qf_constraint_list <- function(x) {
  stopifnot(is.list(x))
  stopifnot(purrr::every(x, is_qf_constraint))
  structure(x, class = c("qf_constraint_list", "list"))
}

is_qf_constraint_list <- function(x) {
  inherits(x, "qf_constraint_list")
}

as_qf_constraint_list <- function(x) {
  UseMethod("as_qf_constraint_list")
}

#' @noRd
#' @export
as_qf_constraint_list.qf_constraint_list <- function(x) {
  x
}
#' @noRd
#' @export
as_qf_constraint_list.list <- function(x) {
  purrr::walk(x, \(cstr) {
    if (!is_qf_constraint(cstr))
      stop("<qf_constraint_list> cannot include a ", typeof(x))
  })
  new_qf_constraint_list(x)
}
#' @noRd
#' @export
as_qf_constraint_list.qf_constraint <- function(x) {
  new_qf_constraint_list(list(x))
}

#' @noRd
#' @export
`&.qf_constraint_list` <- function(e1, e2) {
  new_qf_constraint_list(c(
    as_qf_constraint_list(e1),
    as_qf_constraint_list(e2))
  )
}
#' @noRd
#' @export
`&.qf_constraint` <- `&.qf_constraint_list`



# Methods ======================================================================

#' @noRd
#' @export
print.qf_constraint <- function(x, ...) {
  column_names <- paste(x$cols, collapse = ", ")
  cat0(pretty_class(x), " [", column_names, "]")
  cat("\n")
  invisible(x)
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
new_qf_constraint_specifier <- function(code) {
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

resolve_constraint_specifier <- function(x, table) {
  check_arg_type(x, "qf_constraint")
  check_arg_type(table, "qf_table")

  if (inherits(x, "qf_constraint_specifier"))
    (x)(table)
  else
    x
}

#' @noRd
#' @export
print.qf_constraint_specifier <- function(x, ...) {
  cat("<qf_constraint_specifier>\n")
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

#' Apply the same constraint to multiple columns or sets of columns
#'
#' `apply_to_each()` applies a constraint specifier function
#' ([`cstr_*()`][cstr_]) to each set of columns specified in `...`.
#' `apply_to_each_col()` is similar, but applies the constraint specifier
#' function to each column specified in `.cols`.
#'
#' @param .cstr A constraint specifier function (one of [`cstr_*()`][cstr_])
#' @param ... <[`tidy-select`][args_tidy_select]> Each element is passed to the
#'   `cols` argument of `.cstr`
#' @param .cols <[`tidy-select`][args_tidy_select]> Each column is passed to the
#'   `cols` argument of `.cstr`
#' @param .args List of additional arguments passed to `.cstr`
#'
#' @returns `apply_to_each()` returns a `<qf_constraint_list>` containing
#'   `<qf_constraint_specifier>`s. `apply_to_each_col()` returns a single
#'   `<qf_constraint_specifier>` which in turn returns a `<qf_constraint_list>`.
#'
#' @export
apply_to_each <- function(.cstr, ..., .args = list()) {
  check_arg_type(.cstr, "function")
  check_arg_type(.args, "list")
  col_specs <- rlang::enquos(...)
  constraint_specs <- purrr::map(col_specs, \(spec)
    rlang::eval_tidy(rlang::expr(
      (.cstr)(!!spec, !!!.args)
    ))
  )
  as_qf_constraint_list(constraint_specs)
}

#' @rdname apply_to_each
#' @export
apply_to_each_col <- function(.cstr, .cols, .args = list()) {
  check_arg_type(.cstr, "function")
  check_arg_type(.args, "list")
  col_specs <- rlang::enquo(.cols)

  new_qf_constraint_specifier({
    cols <- select_names(col_specs, .table)
    constraint_objs <- purrr::map(cols, \(col) {
      f <- do.call(.cstr, c(list(col), .args))
      (f)(.table)
    })
    new_qf_constraint_list(constraint_objs)
  })
}


# Constraint checking ==========================================================

#' Check that a constraint is satisfied
#'
#' @param constraint A `<qf_constraint>` object
#' @param table The table to which the constraint is applied
#'
#' @returns A `<qf_check_constraint/qf_check>` object
#'
#' @noRd
check_constraint <- function(constraint, table) {
  results <- data.frame(row.names = seq_len(nrow(table)))
  results$satisfied <- check_constraint_strict(constraint, table)
  results$excepted <- attr(constraint, "exceptions") |>
    purrr::map(excepted_rows, table = table) |>
    purrr::reduce(`|`, .init = rep(FALSE, nrow(table)))
  results$handled <- results$satisfied | results$excepted
  new_qf_check(
    results,
    satisfied = all(results$satisfied),
    handled = all(results$handled),
    constraint = constraint,
    subclass = "qf_check_constraint"
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
  check_arg_type(x, "qf_constraint")
  attr(x, "exceptions")
}

#' @rdname exceptions
#' @export
`exceptions<-` <- function(x, value) {
  check_arg_type(x, "qf_constraint")
  attr(x, "exceptions") <- as_qf_exception_list(value)
  x
}
