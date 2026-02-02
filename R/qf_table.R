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

#' @rdname type-predicates
#' @export
is_qf_table <- function(x) {
  inherits(x, "qf_table")
}

# Methods ======================================================================

#' @noRd
#' @exportS3Method utils::str qf_table
str.qf_table <- function(object, ...) {
  cat("<qf_table> [", nrow(object), " x ", ncol(object), "]\n", sep = "")
  cat("    Columns: ", paste(colnames(object), collapse = ", "), "\n", sep = "")
  cat(
    "    Constraints: ",
    paste(purrr::map(constraints(object),
      \(cstr) paste("<", class(cstr)[[1]], ">", sep = "")
    ), collapse = ", "),
    "\n", sep = ""
  )
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
#' @rdname constraints
#' @export
constraints <- function(x) {
  attr(x, "constraints")
}
#' @rdname constraints
#' @export
`constraints<-` <- function(x, value) {
  if (!rlang::is_list(value)) stop("'value' must be a list")
  attr(x, "constraints") <- purrr::modify(value, \(cstr)
    if (inherits(cstr, "qf_constraint_specifier"))
      (cstr)(x)
    else if (inherits(cstr, "qf_constraint"))
      cstr
    else stop("'value' must contain only <qf_constraint> objects, not ",
      typeof(cstr))
  )
  x
}

#' @rdname check_constraints
#' @exportS3Method check_constraints qf_table
check_constraints.qf_table <- function(x, dataset = NULL, ...) {
  constraints(x) |> purrr::map(check_constraint, x, dataset)
}



