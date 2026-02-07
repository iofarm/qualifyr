# Adapted from doc-tidy-selection.R in the tidyselect package
# https://github.com/r-lib/tidyselect/blob/093f12dbfc26f0aa25892c8ca689689aea5da535/R/doc-tidy-selection.R
# See LICENSE-tidyselect.md for license information

# Mark arguments that use tidy selection using the following tag:
# <[`tidy-select`][args_tidy_select]>

#' Argument type: tidy-select
#'
#' @description
#' Note: This page is adapted from the `args_tidy_select` help topic from the
#' tidyselect package. See LICENSE-tidyselect.md for license information.
#'
#' This page describes the `<tidy-select>` argument modifier which indicates
#' the argument supports **tidy selections**. Tidy selection provides a concise
#' dialect of R for selecting variables based on their names or properties.
#'
#' Various qualifyr functions use tidy selection to select tables from data sets
#' and columns from tables.
#'
#' @section Overview of selection features:
#'
#' Tidyverse selections implement a dialect of R where operators make
#' it easy to select variables:
#'
#' - `:` for selecting a range of consecutive variables.
#' - `!` for taking the complement of a set of variables.
#' - `&` and `|` for selecting the intersection or the union of two sets of
#'   variables.
#' - `c()` for combining selections.
#'
#' In addition, you can use __selection helpers__. Some helpers select specific
#' columns:
#'
#' * [`everything()`][tidyselect::everything]: Matches all variables.
#' * [`last_col()`][tidyselect::last_col]: Select last variable, possibly with
#'   an offset.
#'
#' Other helpers select variables by matching patterns in their names:
#'
#' * [`starts_with()`][tidyselect::starts_with]: Starts with a prefix.
#' * [`ends_with()`][tidyselect::ends_with()]: Ends with a suffix.
#' * [`contains()`][tidyselect::contains()]: Contains a literal string.
#' * [`matches()`][tidyselect::matches()]: Matches a regular expression.
#' * [`num_range()`][tidyselect::num_range()]: Matches a numerical range like
#'   x01, x02, x03.
#'
#' Or from external variables stored in a character vector:
#'
#' * [`all_of()`][tidyselect::all_of()]: Matches variable names in a character
#'   vector. All names must be present, otherwise an out-of-bounds error is
#'   thrown.
#' * [`any_of()`][tidyselect::any_of()]: Same as `all_of()`, except that no
#'   error is thrown for names that don't exist.
#'
#' Or using a predicate function:
#'
#' * [`where()`][tidyselect::where()]: Applies a function to all variables and
#'   selects those for which the function returns `TRUE`.
#'
#' @section Extensions to tidy selection syntax in qualifyr:
#'
#' The `reference` argument of
#' [`cstr_foreign_key()`][qualifyr::cstr_foreign_key()] supports two extensions
#' to tidyselect syntax:
#'
#' * The symbol `.self` refers to the same table
#' * Specific columns from another table can be specified using the syntax
#'   `table[columns]` or `table$column`
#'
#' @seealso `vignette("syntax", package = "tidyselect")`
#'
#' @name args_tidy_select
NULL
