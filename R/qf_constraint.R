# Class definition =============================================================

new_qf_constraint <- function(cols, subclass, ...) {
  stopifnot(is.character(cols))
  stopifnot(is.character(subclass))
  dots <- rlang::enexprs(...)
  stopifnot(all(sapply(names(dots), nchar) > 0))

  structure(cols, class = c(subclass, "qf_constraint"), ...)
}

#' @rdname type-predicates
#' @export
is_qf_constraint <- function(x) {
  inherits(x, "qf_constraint")
}

# Constraint specifier helpers =================================================

#' Create a constraint specifier
#'
#' Constraint declarations (`cstr_*()` functions) are meant to be  called in the
#' RHS of the replacement-form function `constraints<-`. Because replacement-
#' form functions eagerly evaluate the RHS, we cannot use typical rlang-style
#' metaprogramming to enrich the environment of the RHS expressions with
#' dataset and table information. Instead, the `cstr_*()` functions return
#' closures that enclose their tidy-select specifications and return  constraint
#' objects when given context information.
#'
#' @param code A raw expression to be used as the body of the constraint
#'   specifier function
#'
#' @returns A new closure of class `<qf_constraint_specifier>` that takes two
#'   arguments, `.dataset` and `.table`, and returns a `<qf_constraint>` object.
#'
#' @noRd
new_constraint_specifier <- function(code) {
  structure(
    rlang::new_function(
      alist(.dataset = , .table = ),
      body = rlang::enexpr(code),
      env = rlang::caller_env()
    ),
    class = "qf_constraint_specifier"
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
#' @returns A list with two elements, each a tidyselect expression: `$table`, the
#'   expression specifying the table, and `$cols`, the expression specifying the
#'   columns.
#'
#' @details To select columns from a specific table, use either:
#'
#'   * `table$column`, where `table` and `column` are tidyselect specifiers for
#'   the table and columns, respectively; or
#'   * `table[column1, column2, ...]` where `table` specifies the table and
#'   `column1, column2, ...` specify columns from `table`
#'
#' If `expr` is not a call to `$` or `[`, then it will be returned as `$table`
#' while `$col` is `NULL`.
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

# Constraint declarations ======================================================

# declare data pronouns to avoid R CMD CHECK notes
utils::globalVariables(c(".dataset", ".table"))

#' @name cstr_
#' @rdname cstr_
#'
#' @title Constraints for relational data structures
#'
#' @description Functions to specify constraints for qualifyr data frames. The
#'   return values should be stored in a list and passed to `constraints<-`.
#'
#' @param cols <`tidy-select`> The columns to set constraints on
#' @param reference <`tidy-select`> For `cstr_foreign_key()`, the table and columns to
#'   reference. If the key columns and reference columns have the same names,
#'   you can specify just the table. Otherwise, specify the columns using
#'   `table[columns]` where `columns` is a tidy-select specification.
#'
#' @returns These functions return closures of class
#'   `<qf_constraint_specifier>`, which take a qualifyr data set and data frame
#'   as arguments and return an object inheriting from `<qf_constraint>`. This
#'   unfortunate implementation detail allows omitting the data set and table
#'   when used in the RHS of the replace-form function `constraints<-`.
NULL

#' @rdname cstr_
#' @export
cstr_unique_key <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  new_constraint_specifier({
    cols_chr <- select_names(cols_quo, .table)
    new_qf_constraint(cols_chr, "cstr_unique_key")
  })
}

#' @rdname cstr_
#' @export
cstr_primary_key <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  new_constraint_specifier({
    cols_chr <- select_names(cols_quo, .table)
    new_qf_constraint(cols_chr, c("cstr_primary_key", "cstr_unique_key"))
  })
}

#' @rdname cstr_
#' @export
cstr_foreign_key <- function(cols, reference) {
  cols_quo <- rlang::enquo(cols)
  ref_quo  <- rlang::enquo(reference)
  ref_exprs <- parse_reference_specifier(rlang::quo_squash(ref_quo))
  ref_env <- rlang::quo_get_env(ref_quo)
  ref_cols_quo <-
    if (is.null(ref_exprs$cols)) cols_quo
    else rlang::new_quosure(ref_exprs$cols, ref_env)
  ref_table_quo <- rlang::new_quosure(ref_exprs$table, ref_env)

  new_constraint_specifier({
    cols_chr <- select_names(cols_quo, .table)
    ref_table_chr <- select_names(ref_table_quo, as.list(.dataset))
    if (length(ref_table_chr) != 1) stop("A foreign key must have a single
      reference table, but ", length(ref_table_chr), " were selected")
    ref_table_obj <- .dataset[[ref_table_chr]]
    ref_cols_chr <- select_names(ref_cols_quo, ref_table_obj)
    ref_is_unique_key <- constraints(ref_table_obj) |>
      purrr::some(\(constraint)
        inherits(constraint, "cstr_unique_key") &&
        setequal(constraint, ref_cols_chr)
      )
    if (!ref_is_unique_key) stop("Reference columns must be a unique key")

    new_qf_constraint(
      cols_chr,
      ref_table = ref_table_chr, ref_cols = ref_cols_chr,
      subclass = "cstr_foreign_key"
    )
  })
}

# Check constraints ============================================================

check_constraint <- function(constraint, dataframe, dataset) {
  UseMethod("check_constraint")
}

#' @noRd
#' @exportS3Method check_constraint cstr_unique_key
check_constraint.cstr_unique_key <- function(constraint, dataframe, dataset) {
  key_cols <- dataframe[as.character(constraint)]

  is_duplicated <- duplicated(key_cols) | duplicated(key_cols, fromLast = TRUE)

  result <- !is_duplicated
  attr(result, "constraint") <- constraint
  result
}

#' @noRd
#' @exportS3Method check_constraint cstr_primary_key
check_constraint.cstr_primary_key <- function(constraint, dataframe, dataset) {
  key_cols <- dataframe[as.character(constraint)]

  is_na <- key_cols |> purrr::map(is.na) |> purrr::reduce(`|`)

  result <- NextMethod() & (!is_na)
  result
}

#' @noRd
#' @exportS3Method check_constraint cstr_foreign_key
check_constraint.cstr_foreign_key <- function(constraint, dataframe, dataset) {
  key_cols <- dataframe[as.character(constraint)]
  ref_table_chr <- attr(constraint, "ref_table")
  ref_table <-
    if (is.null(ref_table_chr)) dataframe
    else dataset[[ref_table_chr]]
  ref_cols <- ref_table[attr(constraint, "ref_cols")]

  matches <- 1:ncol(key_cols) |>
    purrr::map(\(j) match(key_cols[j], ref_cols[j])) |>
    purrr::reduce(`&`)

  result <- matches
  attr(result, "constraint") <- constraint
  result
}
