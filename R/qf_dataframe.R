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

#' @rdname type-predicates
#' @export
is_qf_dataframe <- function(x) {
  inherits(x, "qf_dataframe")
}

#' @rdname constraints
#' @exportS3Method constraints qf_dataframe
constraints.qf_dataframe <- function(x, ...) {
  attr(x, "constraints")
}
#' @rdname constraints
#' @exportS3Method "constraints<-" qf_dataframe
`constraints<-.qf_dataframe` <- function(x, dataset = NULL, ..., value) {
  if (!rlang::is_list(value)) stop("'value' must be a list")
  if (!is.null(dataset)) if (!is_qf_dataset(dataset)) stop("'dataset' must be
    NULL or a <qf_dataset> object")

  constraints_objects <- value |> purrr::modify(\(cstr) {
    if (is_qf_constraint(cstr)) {
      cstr
    } else if (
      rlang::is_function(cstr) &&
      inherits(cstr, "qf_constraint_specifier")
    ) {
      cstr(.dataset = dataset, .table = x)
    } else stop("Elements of 'value' must be either constraint objects or
      constraint specifiers returned by cstr_* functions, not ",
      typeof(cstr))
  })

  attr(x, "constraints") <- constraints_objects
  x
}

#' @rdname check_constraints
#' @exportS3Method check_constraints qf_dataframe
check_constraints.qf_dataframe <- function(x, dataset = NULL, ...) {
  constraints(x) |> purrr::map(check_constraint, x, dataset)
}



