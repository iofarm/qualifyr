# Class definition =============================================================

new_qf_table <- function(x, constraints) {
  stopifnot(is.data.frame(x))
  if (!is.null(constraints)) stopifnot(
    is.list(constraints) &&
    all(sapply(constraints, is_qf_constraint))
  )

  structure(x,
    class = c("qf_table", attr(x, "class")),
    "constraints" = constraints
  )
}

validate_qf_table <- function(x) {
  if (!is.null(attr(x, "context")) && !is_qf_dataset(attr(x, "context")))
    stop("Table has invalid 'context' attribute")
  purrr::walk(constraints(x), \(cstr) tryCatch(
    validate_qf_constraint(cstr, x),
    error = \(e) stop(
      pretty_class(cstr), " (", paste(cstr$cols, sep = ", "), ") ",
      "has invalid structure: \n",
      e$message
    )
  ))
  x
}

#' Convert a data frame to a qualifyr table
#'
#' Create a qualifyr table object from a data frame
#'
#' @param x Any object inheriting from class `<data.frame>`
#'
#' @returns A copy of `x` with class `<qf_table>` (retaining previous class
#'   information as superclasses) and the `constraints` attribute set to an
#'   empty list.
#'
#' @export
as_qf_table <- function(x) {
  if (is_qf_table(x)) x
  else {
    if (!inherits(x, "data.frame")) {
      x <- as.data.frame(x)
    }
    new_qf_table(x, list())
  }
}

#' @rdname type_predicates
#' @export
is_qf_table <- function(x) {
  inherits(x, "qf_table")
}



# Methods ======================================================================

#' @rdname check_constraints
#' @export
check_constraints.qf_table <- function(x) {
  constraints(x) |> purrr::map(check_constraint, x)
}

#' @noRd
#' @exportS3Method utils::str
str.qf_table <- function(object,
  show.context = TRUE,
  nest.lev = 0,
  indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."),
  ...
) {
  dots <- list(...)
  context <- attr(object, "context")
  attr(object, "context") <- NULL

  # Header
  cat("<qf_table>\n")

  # Columns / data
  cat(indent.str, "- Data:", sep = "")
  NextMethod(
    nest.lev = nest.lev + 1,
    indent.str = paste(rep.int(" ", max(0, nest.lev + 2)),
      collapse = ".."),
    give.attr = FALSE
  )

  # Constraints
  cat(indent.str, "- Constraints:", "\n", sep = "")
  constraint_types <- constraints(object) |>
    vapply(pretty_class, character(1)) |>
    format()
  constraint_cols <- constraints(object) |>
    vapply(\(cstr) {
      col_names <- paste0("[", paste(cstr$cols, collapse = ", "), "]")
      if (inherits(cstr, "cstr_foreign_key")) {
        ref_col_names <- paste(cstr$ref_cols, collapse = ", ")
        paste0(col_names, " => ", cstr$ref_table, "[", ref_col_names, "]")
      } else {
        col_names
      }
    }, character(1))
  paste0(indent.str, "..", " - ", constraint_types, " ", constraint_cols) |>
    cat(sep = "\n")

  # Context
  if (show.context && !is.null(context)) {
    cat(indent.str, "- Context: ", pretty_class(context), ": ", sep = "")
    cat(names(context), sep = ", ")
    cat("\n")
  }

  invisible(object)
}

#' @noRd
#' @export
print.qf_table <- function(x, ...) {
  str(x)
}



# Constraint handling ==========================================================

#' Get or set constraints of a qualifyr table
#'
#' @param x A qualifyr table to set constraints on
#' @param value A list of constraints (or constraint specifiers), as returned
#'   by `cstr_*()` functions.
#'
#' @returns For `constraints`, the list of `<qf_constraint>` objects associated
#'   with the table. For `constraints<-`, an updated version of `x` with
#'   constraints set.
#'
#' @details If `value` contains foreign key specifiers referencing another
#'   table, `x` must have the `context` attribute set, otherwise `constraints<-`
#'   will signal an error. Tables returned by subsetting a qualifyr dataset
#'   using `$`, `[[`, or `[` have this attribute set.
#'
#' @seealso [pick_constraint]
#'
#' @rdname constraints
#' @export
constraints <- function(x) {
  attr(x, "constraints")
}
#' @rdname constraints
#' @export
`constraints<-` <- function(x, value) {
  if (!is_qf_table(x)) stop("'x' must be a qualifyr table, not ", typeof(x))
  if (!rlang::is_list(value)) stop("'value' must be a list, not ",
    typeof(value))
  attr(x, "constraints") <- purrr::modify(value, as_qf_constraint, table = x)
  validate_qf_table(x)
}

#' @name pick_constraint
#' @rdname pick_constraint
#' @order 0
#'
#' @title Get or replace a specific constraint
#'
#' @description These functions let you identify a specific constraint on a
#'   table for the purpose of inspecting, modifying, or replacing it.
#'
#' @param table The table to search for the constraint
#' @param cols <[`tidy-select`][args_tidy_select]> The columns the constraint
#'   applies to. If `cols` is `NULL` or otherwise identifies zero columns,
#'   returns the constraint with the specified type if there is exactly one, and
#'   throws an error otherwise.
#' @param value The constraint object to replace the specified constraint with
#'
#' @returns For the selection forms, the constraint object. For the replacement
#'   forms, the updated table object.
#'
NULL

#' Get the index of a specific constraint
#'
#' @inheritParams pick_constraint
#' @param class a string naming the class of the constraint to pick
#'
#' @returns the index of the constraint
#'
#' @noRd
constraint_index <- function(table, cols = NULL, class) {
  matching_class <- purrr::map_lgl(constraints(table), \(cstr)
    class(cstr)[[1]] == class
  )
  query_cols <- select_names(rlang::enquo(cols), table)
  matching_cols <-
    if (length(query_cols) == 0) {
      rep(TRUE, length(constraints(table)))
    } else {
      purrr::map_lgl(constraints(table), \(cstr)
        setequal(query_cols, cstr$cols)
      )
    }
  matches <- which(matching_class & matching_cols)
  if (length(matches) == 0) stop(deparse(sys.call(-1)), " does not match any
    constraints")
  else if (length(matches) > 1) stop(deparse(sys.call(-1)), " is ambiguous,
    i.e., matches multiple constraints")
  else matches
}

#' Get or replace a specific constraint using the class name
#'
#' @inheritParams pick_constraint
#' @param class a string naming the class of the constraint to pick
#'
#' @inheritSection pick_constraint returns
#'
#' @noRd
constraint <- function(table, cols = NULL, class) {
  idx <- constraint_index(table, {{ cols }}, class)
  constraints(table)[[idx]]
}
`constraint<-` <- function(table, cols = NULL, class, value) {
  idx <- constraint_index(table, {{ cols }}, class)
  constraints(table)[[idx]] <- as_qf_constraint(value, table = table)
  validate_qf_table(table)
}
