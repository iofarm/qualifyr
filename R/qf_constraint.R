# Class definition =============================================================

new_qf_constraint <- function(x, subclass) {
  stopifnot(is.list(x))
  stopifnot(is.character(subclass))

  structure(x, class = c(subclass, "qf_constraint"))
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
utils::globalVariables(c(".table"))

#' @name cstr_
#' @rdname cstr_
#'
#' @title Constraints for relational data structures
#'
#' @description Functions to specify constraints for qualifyr tables. The
#'   return values should be stored in a list and passed to `constraints<-`.
#'
#' @param cols <`tidy-select`> The columns to set constraints on
#' @param reference <`tidy-select`> For `cstr_foreign_key()`, the table and
#'   columns to reference. If the key columns and reference columns have the
#'   same names, you can specify just the table. Otherwise, specify the columns
#'   using `table[columns]` where `columns` is a tidy-select specification.
#'
#' @returns These functions return closures of class
#'   `<qf_constraint_specifier>`, which take a qualifyr table and return an
#'   object inheriting from `<qf_constraint>`. This unfortunate implementation
#'   detail allows omitting the data set and table when used in the RHS of the
#'   replace-form function `constraints<-`, making front-end syntax a little
#'   nicer.
NULL

#' @rdname cstr_
#' @export
cstr_unique_key <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  new_constraint_specifier({
    new_qf_constraint(
      list(cols = select_names(cols_quo, .table)),
      "cstr_unique_key"
    )
  })
}

#' @rdname cstr_
#' @export
cstr_primary_key <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  new_constraint_specifier({
    new_qf_constraint(
      list(cols = select_names(cols_quo, .table)),
      c("cstr_primary_key", "cstr_unique_key")
    )
  })
}

#' @rdname cstr_
#' @export
cstr_foreign_key <- function(cols, reference) {
  cols_quo <- rlang::enquo(cols)
  ref_quo  <- rlang::enquo(reference)
  ref_exprs <- parse_reference_specifier(rlang::quo_squash(ref_quo))
  ref_env <- rlang::quo_get_env(ref_quo)
  ref_table_quo <- rlang::new_quosure(ref_exprs$table, ref_env)
  ref_cols_quo <-
    if (is.null(ref_exprs$cols)) cols_quo
    else rlang::new_quosure(ref_exprs$cols, ref_env)

  new_constraint_specifier({
    cols_chr <- select_names(cols_quo, .table)
    ref_table_chr <- NULL
    ref_table_obj <- NULL
    if (rlang::quo_squash(ref_table_quo) == rlang::sym(".self")) {
      ref_table_chr <- ".self"
      ref_table_obj <- .table
    } else {
      if (is.null(attr(.table, "context"))) stop("'reference' refers to a separate
      table, but no information on other tables in the dataset was found")
      ref_table_chr <- select_names(
        ref_table_quo,
        attr(.table, "context")
      )
      if (length(ref_table_chr) != 1) stop("A foreign key must have a single
      reference table, but ", length(ref_table_chr), " were selected")
      ref_table_obj <- attr(.table, "context")[[ref_table_chr]]
    }
    ref_cols_chr <- select_names(ref_cols_quo, ref_table_obj)

    new_qf_constraint(
      list(
        cols = cols_chr,
        ref_table = ref_table_chr,
        ref_cols = ref_cols_chr
      ),
      "cstr_foreign_key"
    )
  })
}


# Validate constraints =========================================================

validate_constraint <- function(constraint, table) {
  UseMethod("validate_constraint")
}

#' @noRd
#' @exportS3Method validate_constraint qf_constraint
validate_constraint.qf_constraint <- function(constraint, table) {
  if (!setequal(constraint$cols, unique(constraint$cols)))
    stop(pretty_class(constraint), " includes duplicate columns")
  if (!all(constraint$cols %in% colnames(table)))
    stop(pretty_class(constraint), " includes non-existent columns: ",
      paste(setdiff(constraints$cols, colnames(table)), collapse = ", "))
}

#' @noRd
#' @exportS3Method validate_constraint cstr_unique_key
validate_constraint.cstr_unique_key <- function(constraint, table) {
  NextMethod()
}

#' @noRd
#' @exportS3Method validate_constraint cstr_primary_key
validate_constraint.cstr_primary_key <- function(constraint, table) {
  NextMethod()
}

#' @noRd
#' @exportS3Method validate_constraint cstr_foreign_key
validate_constraint.cstr_foreign_key <- function(constraint, table) {
  NextMethod()
  ref_table_obj <- resolve_table_reference(table, constraint$ref_table)
  if (is.null(ref_table_obj))
    stop(pretty_class(constraint), " references a non-existent table: ",
      constraint$ref_table_obj)
  references_unique_key <- purrr::some(constraints(ref_table_obj), \(cstr)
    inherits(cstr, "cstr_unique_key") && setequal(constraint$cols, cstr$cols)
  )
  if (!references_unique_key)
    stop(pretty_class(constraint), " does not reference a unique key")
}

# Check constraints ============================================================

check_constraint <- function(constraint, table) {
  UseMethod("check_constraint")
}

#' @noRd
#' @exportS3Method check_constraint cstr_unique_key
check_constraint.cstr_unique_key <- function(constraint, table) {
  key_cols <- table[constraint$cols]

  is_duplicated <- duplicated(key_cols) | duplicated(key_cols, fromLast = TRUE)

  result <- !is_duplicated
  attr(result, "constraint") <- constraint
  result
}

#' @noRd
#' @exportS3Method check_constraint cstr_primary_key
check_constraint.cstr_primary_key <- function(constraint, table) {
  key_cols <- table[constraint$cols]

  is_na <- key_cols |> purrr::map(is.na) |> purrr::reduce(`|`)

  result <- NextMethod() & (!is_na)
  result
}

#' @noRd
#' @exportS3Method check_constraint cstr_foreign_key
check_constraint.cstr_foreign_key <- function(constraint, table) {
  key_cols <- table[constraint$cols]
  ref_table_chr <- constraint$ref_table
  ref_table <-
    if (ref_table_chr == ".self") table
    else {
      if (is.null(attr(table, "context"))) stop("Foreign key references a
        separate table, but no information on other tables in the dataset was
        found")
      attr(table, "context")[[ref_table_chr]]
    }
  ref_cols <- ref_table[constraint$ref_cols]

  matches <- 1:ncol(key_cols) |>
    purrr::map(\(j) match(key_cols[j], ref_cols[j])) |>
    purrr::reduce(`&`)

  result <- matches
  attr(result, "constraint") <- constraint
  result
}

