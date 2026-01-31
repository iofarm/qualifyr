test_that("setting constraints works", {
  dset <- qf_dataset(
    flights = nycflights13::flights,
    planes = nycflights13::planes
  )

  constraints(dset, planes) <- list(
    cstr_primary_key(tailnum)
  )
  expect_identical(
    constraints(dset$planes),
    list(structure(
      "tailnum",
      class = c("cstr_primary_key", "cstr_unique_key", "qf_constraint")
    ))
  )

  constraints(dset, flights) <- list(
    cstr_primary_key(c(flight, year, month, day)),
    cstr_foreign_key(tailnum, reference = planes)
  )
  expect_identical(
    constraints(dset$flights),
    list(
      structure(
        c("flight", "year", "month", "day"),
        class = c("cstr_primary_key", "cstr_unique_key", "qf_constraint")
      ),
      structure(
        "tailnum",
        ref_table = "planes", ref_cols = "tailnum",
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
  constraints(dset, planes) <- list(cstr_primary_key(tailnum))

  dset1 <- dset
  constraints(dset1, flights) <- list(
    cstr_foreign_key(tailnum, reference = planes)
  )
  dset2 <- dset
  constraints(dset2, flights) <- list(
    cstr_foreign_key(tailnum, reference = planes$tailnum)
  )
  dset3 <- dset
  constraints(dset3, flights) <- list(
    cstr_foreign_key(tailnum, reference = planes[tailnum])
  )
  expect_identical(constraints(dset1, flights), constraints(dset2, flights))
  expect_identical(constraints(dset2, flights), constraints(dset3, flights))

})
