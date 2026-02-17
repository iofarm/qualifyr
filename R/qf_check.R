# Class definition =============================================================

new_qf_check <- function(x, subclass, satisfied, handled, ...) {
  structure(
    x,
    class = c(subclass, "qf_check"),
    satisfied = satisfied, handled = handled,
    ...
  )
}



# Accessors ====================================================================

#' Inspect constraint check results
#'
#' Functions to access attributes of constraint check results
#'
#' @param x An object returned by [`check_constraints()`], i.e., a `<qf_check`>
#'   object
#' @param ... Not used
#'
#' @returns Logical; for `satisfied()`, whether all constraints were satisfied,
#'   and for `handled()`, whether all constraints were either satisfied or
#'   excepted. `as.logical(x)` is equivalent to `handled(x)`.
#'
#' @name inspect_results
NULL


#' @rdname inspect_results
#' @export
satisfied <- function(x) {
  attr(x, "satisfied")
}
#' @rdname inspect_results
#' @export
handled <- function(x) {
  attr(x, "handled")
}
#' @rdname inspect_results
#' @export
as.logical.qf_check <- function(x, ...) {
  handled(x)
}



# Pretty printing ==============================================================

#' @noRd
#' @export
print.qf_check <- function(x, max_width = getOption("width"), ...) {
  print(attr(x, "constraint"))
  if (satisfied(x)) {
    cat("   All rows satisfied")
  } else if (handled(x)) {
    cat("   All rows satisfied or excepted")
  } else {
    message <- "   Violating rows: "
    indices <- format_indices(
      which(!x$handled),
      max_width - nchar(message)
    )
    cat0(message, indices)
  }
  cat("\n")
  invisible(x)
}

#' @noRd
#' @export
print.qf_check_table <- function(x, ...) {
  check_header(x)
  for (cstr in x) print(cstr)
  invisible(x)
}

#' @noRd
#' @export
print.qf_check_dataset <- function(x, ...) {
  check_header(unlist(x, recursive = FALSE))
  indent <- "   "
  for (i in 1:length(x)) {
    if (!purrr::every(x[[i]], handled)) {
      cat0("=> In table '", names(x)[[i]], "':\n")
      for (j in 1:length(x[[i]])) {
        if (!handled(x[[i]][[j]])) {
          constraint_check_text <- utils::capture.output({
            cat0("[[", j, "]] ")
            print(x[[i]][[j]], max_width = getOption("width") - nchar(indent))
          })
          cat(paste0(indent, constraint_check_text), sep = "\n")
        }
      }
    }
  }
}

check_header <- function(x) {
  satisfied <- purrr::map_lgl(x, satisfied)
  handled <- purrr::map_lgl(x, handled)
  n_satisfied <- sum(satisfied)
  n_excepted <- sum(handled) - sum(satisfied)
  n_violated <- length(x) - sum(handled)
  cat("Constraint check report:", n_satisfied, "satisfied", "/", n_excepted,
    "excepted", "/", n_violated, "violated:", "\n")
}


