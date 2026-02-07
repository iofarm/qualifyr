
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qualifyr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/iofarm/qualifyr/graph/badge.svg)](https://app.codecov.io/gh/iofarm/qualifyr)
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

dset <- qf_dataset(airlines, flights)

constraints(dset$airlines) <- list(
  cstr_primary_key(carrier),
  cstr_not_missing(name)
)
constraints(dset$flights) <- list(
  cstr_primary_key(c(year, month, day, carrier, flight)),
  cstr_foreign_key(carrier, airlines)
)

results <- check_constraints(dset)
```
