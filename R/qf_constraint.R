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

#' Get or set constraints of a qualifyr data frame
#'
#' @param x A qualifyr data set or data frame to set constraints on
#' @param table If `x` is a data set, the name of the table to set constraints
#'   on, either as a character string or bare symbol
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
#' @export
constraints <- function(x, ...) {
  UseMethod("constraints")
}
#' @rdname constraints
#' @export
`constraints<-` <- function(x, ..., value) {
  UseMethod("constraints<-")
}

check_constraint <- function(constraint, dataframe, dataset) {
  UseMethod("check_constraint")
}

cstr_unique_key <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  structure(function(set, frame) {
    cols_chr <-
      names(tidyselect::eval_select(cols_quo, frame, allow_rename = FALSE))
    new_qf_constraint(cols_chr, "cstr_unique_key")
  }, class = "cstr_builder")
}

cstr_primary_key <- function(cols) {
  cols_quo <- rlang::enquo(cols)
  structure(function(set, frame) {
    cols_chr <-
      names(tidyselect::eval_select(cols_quo, frame, allow_rename = FALSE))
    new_qf_constraint(cols_chr, c("cstr_primary_key", "cstr_unique_key"))
  }, class = "cstr_builder")
}

cstr_foreign_key <- function(cols, ref_cols) {
  cols_quo <- rlang::enquo(cols)
  ref_cols_quo <- rlang::enquo(ref_cols)
  ref_cols_expr <- rlang::quo_get_expr(ref_cols_quo)
  ref_table_quo <- NULL
  if (is.call(ref_cols_expr) && ref_cols_expr[[1]] == "%from%") {
    ref_cols_quo <- rlang::new_quosure(ref_cols_expr[[2]],
      rlang::quo_get_env(ref_cols_quo))
    ref_table_quo <- rlang::new_quosure(ref_cols_expr[[3]],
      rlang::quo_get_env(ref_cols_quo))
  }

  structure(function(set, frame) {
    cols_chr <- select_names(cols_quo, frame)
    ref_table_chr <- if (is.null(ref_table_quo)) NULL
      else select_names(ref_table_quo, as.list(set))
    ref_cols_chr <- select_names(ref_cols_quo,
      if (is.null(ref_table_chr)) frame else set[[ref_table_chr]]
    )

    stopifnot(length(cols_chr) == length(ref_cols_chr))
    stopifnot(purrr::some(constraints(set[[ref_table_chr]]), \(cstr)
      inherits(cstr, "cstr_unique_key") && setequal(cstr, ref_cols_chr)
    ))

    new_qf_constraint(
      cols_chr,
      ref_cols = ref_cols_chr,
      ref_table = ref_table_chr,
      subclass = "cstr_foreign_key"
    )
  }, class = "cstr_builder")
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

`%from%` <- function(cols, table) {
  stop("%from% can only be used within a cstr_*() function to specify columns
    from another table")
}

select_names <- function(expr, data, ...) {
  names(tidyselect::eval_select(expr, data, allow_rename = FALSE, ...))
}
