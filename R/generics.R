#' Check constraints on qualifyr data set or table
#'
#' Checks that a data set or table satisfies the constraints set on it.
#'
#' @param x A `<qf_dataset>` or `<qf_table>` object.
#'
#' @returns If `x` is a `<qf_table>` object, a list where each entry is the
#'   results from checking one constraint. If `x` is a `<qf_dataset>` object, a
#'   list where each entry is the results from checking constraints on one
#'   table. The result from checking one constraint takes the form of a list
#'   with the elements:
#'
#' * `$satsified` - A logical indicating whether the constraint was satisfied,
#'   not considering exceptions
#' * `$handled` - A logical indicating whether every row either satisfied the
#'   constraint or was excepted
#' * `$rows` - A list of logical vectors indicating whether each row:
#'   * `.. $satisfied` - satisfied the condition
#'   * `.. $excepted` - was excepted
#'   * `.. $handled` - satisfied the condition or was excepted
#'
#' @details If `x` is a `<qf_table>` with a foreign key that references another
#'   table, then `x` must have the `context` attribute set. The `context`
#'   attribute is set when `x` is accessed by subsetting a `<qf_dataset>`
#'   object.
#'
#' @export
check_constraints <- function(x) {
  UseMethod("check_constraints")
}
