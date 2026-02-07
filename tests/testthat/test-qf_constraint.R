test_that("setting constraints works", {
  dset <- qf_dataset(
    flights = nycflights13::flights,
    planes = nycflights13::planes
  )

  constraints(dset$planes) <- list(
    cstr_primary_key(tailnum)
  )
  expect_identical(
    constraints(dset$planes),
    list(
      structure(
        list(cols = "tailnum"),
        class = c("cstr_primary_key", "cstr_unique_key", "qf_constraint")
      )
    )
  )

  constraints(dset$flights) <- list(
    cstr_primary_key(c(flight, year, month, day)),
    cstr_foreign_key(tailnum, reference = planes)
  )
  expect_identical(
    constraints(dset$flights),
    list(
      structure(
        list(cols = c("flight", "year", "month", "day")),
        class = c("cstr_primary_key", "cstr_unique_key", "qf_constraint")
      ),
      structure(
        list(cols = "tailnum", ref_table = "planes", ref_cols = "tailnum"),
        class = c("cstr_foreign_key", "qf_constraint")
      )
    )
  )
})

test_that("alternative reference specification formats are equivalent", {

  dset <- qf_dataset(
    flights = nycflights13::flights,
    planes = nycflights13::planes
  )
  constraints(dset$planes) <- list(cstr_primary_key(tailnum))

  dset1 <- dset
  constraints(dset1$flights) <- list(
    cstr_foreign_key(tailnum, reference = planes)
  )
  dset2 <- dset
  constraints(dset2$flights) <- list(
    cstr_foreign_key(tailnum, reference = planes$tailnum)
  )
  dset3 <- dset
  constraints(dset3$flights) <- list(
    cstr_foreign_key(tailnum, reference = planes[tailnum])
  )
  expect_identical(constraints(dset1$flights), constraints(dset2$flights))
  expect_identical(constraints(dset2$flights), constraints(dset3$flights))

})

test_that("constraint checking works", {

  dset <- withr::with_package("nycflights13",
    qf_dataset(airlines, flights)
  )

  constraints(dset$airlines) <- list(
    cstr_primary_key(carrier),
    cstr_not_missing(name)
  )
  constraints(dset$flights) <- list(
    cstr_primary_key(c(year, month, day, carrier, flight)),
    cstr_foreign_key(carrier, airlines)
  )

  # Case 1: Primary key on 'flights' is invalid because of duplicate keys;
  #   otherwise OK

  result <- check_constraints(dset)
  expect_true(result$airlines[[1]]$satisfied)
  expect_true(result$airlines[[2]]$satisfied)
  expect_false(result$flights[[1]]$satisfied)
  expect_true(result$flights[[2]]$satisfied)

  # Case 2: Primary key and not-missing constraint on 'airlines' is invalid
  #   because of missing values for Delta Airlines; Foreign key on 'flights' is
  #   invalid because it references this primary key.

  dset2 <- dset
  dset2$airlines[dset$airlines$carrier == "DL", ] <- NA

  result2 <- check_constraints(dset2)
  expect_false(result2$airlines[[1]]$satisfied)
  expect_false(result2$airlines[[2]]$satisfied)
  expect_false(result2$flights[[1]]$satisfied)
  expect_false(result2$flights[[2]]$satisfied)

})

test_that("constraint validators work", {
  dset <- withr::with_package("nycflights13",
    qf_dataset(airlines, flights)
  )

  constraints(dset$airlines) <- list(
    cstr_not_missing(name)
  )
  expect_error(regexp = "includes non-existent columns", {
    not_missing(dset$airlines)$cols <- "nombre"
  })
  expect_error(regexp = "does not reference a unique key", {
    constraints(dset$flights) <- list(
      cstr_primary_key(c(year, month, day, carrier, flight)),
      cstr_foreign_key(carrier, airlines)
    )
  })
})
