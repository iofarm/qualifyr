#' @describeIn cstr_ Requires that set of values in the key matches a set of
#'   values in the reference columns. The reference columns must be a unique key
#'   or a primary key.
#' @order 3
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

#' @noRd
#' @export
validate_qf_constraint.cstr_foreign_key <- function(x, table) {
  ref_table_obj <- resolve_table_reference(table, x$ref_table)
  if (is.null(ref_table_obj)) stop(pretty_class(x), " references a ",
    "non-existent table: ", x$ref_table)
  references_unique_key <- purrr::some(constraints(ref_table_obj), \(cstr)
    inherits(cstr, "cstr_unique_key") && setequal(x$cols, cstr$cols)
  )
  if (!references_unique_key) stop(pretty_class(x), " does not reference a ",
    "unique key")
  NextMethod()
}

#' @noRd
#' @export
check_constraint_strict.cstr_foreign_key <- function(constraint, table) {
  key_cols <- table[constraint$cols]
  ref_table_chr <- constraint$ref_table
  ref_table <- resolve_table_reference(table, ref_table_chr)
  ref_cols <- ref_table[constraint$ref_cols]

  result <- 1:ncol(key_cols) |>
    purrr::map(\(j) match(key_cols[[j]], ref_cols[[j]])) |>
    purrr::pmap_lgl(\(...) length(setdiff(unique(c(...)), NA)) == 1)

  attr(result, "constraint") <- constraint
  result
}

#' @rdname pick_constraint
#' @order 3
#' @export
foreign_key <- function(table, cols = NULL) {
  constraint(table, {{ cols }}, "cstr_foreign_key")
}
#' @rdname pick_constraint
#' @order 13
#' @export
`foreign_key<-` <- function(table, cols = NULL, value) {
  constraint(table, {{ cols }}, "cstr_foreign_key") <- value
  table
}
