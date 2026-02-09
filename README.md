
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qualifyr

<!-- badges: start -->

[![R-CMD-check](https://github.com/iofarm/qualifyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/iofarm/qualifyr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A framework for representing relational data structures in R, and
flexible tools for defining and validating constraints on relational
data.

## Installation

You can install the development version of qualifyr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("iofarm/qualifyr")
```

## Example

``` r
library(qualifyr)
# install.packages("nycflights13")
library(nycflights13)

nycflights <- qf_dataset(airlines, planes, flights)

constraints(nycflights$airlines) <-
  cstr_primary_key(carrier) &
  cstr_not_missing(name)
constraints(nycflights$planes) <-
  cstr_primary_key(tailnum)
constraints(nycflights$flights) <- 
  cstr_primary_key(c(year, month, day, carrier, flight)) &
  cstr_foreign_key(carrier, airlines) &
  cstr_foreign_key(tailnum, planes)

check_constraints(nycflights)
#> Constraint check report: 4 satisfied / 0 excepted / 2 violated: 
#> => In table 'flights':
#>    [[1]] <cstr_primary_key> [year, month, day, carrier, flight]
#>       Violating rows: 228756, 229231, 235372, 235857, 242047... (43 more)
#>    [[3]] <cstr_foreign_key> [tailnum] => planes[tailnum]
#>       Violating rows: 10, 15, 19, 22, 26-27, 32, 35, 37, 39, 41... (52595 more)
```
