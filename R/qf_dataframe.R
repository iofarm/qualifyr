new_qf_dataframe <- function(x, constraints) {
  stopifnot(is.data.frame(x))
  if (!is.null(constraints)) stopifnot(
    is.list(constraints) &&
    all(sapply(constraints, is_qf_constraint))
  )

  structure(x,
    class = c("qf_dataframe", attr(x, "class")),
    "constraints" = constraints
  )
}

as_qf_dataframe <- function(x) {
  if (!inherits(x, "data.frame")) {
    x <- as.data.frame(x)
  }
  new_qf_dataframe(x, list())
}

is_qf_dataframe <- function(x) {
  inherits(x, "qf_dataframe")
}

#' @noRd
#' @exportS3Method constraints qf_dataframe
constraints.qf_dataframe <- function(x, ...) {
  attr(x, "constraints")
}
#' @noRd
#' @exportS3Method "constraints<-" qf_dataframe
`constraints<-.qf_dataframe` <- function(x, ..., value) {
  attr(x, "constraints") <- value
  x
}
#' @noRd
#' @exportS3Method check_constraints qf_dataframe
check_constraints.qf_dataframe <- function(x, dataset, ...) {
  constraints(x) |> purrr::map(check_constraint, x, dataset)
}



