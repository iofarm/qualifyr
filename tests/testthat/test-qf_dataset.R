test_that("setting constraints works", {

  dset <- qf_dataset(
    flights = nycflights13::flights,
    planes = nycflights13::planes
  )

  constraints(dset, planes) <- list(
    cstr_primary_key(tailnum)
  )
  expect_length(constraints(dset$planes), 1)
  constraints(dset, flights) <- list(
    cstr_primary_key(c(flight, year, month, day)),
    cstr_foreign_key(tailnum, reference = planes)
  )
  expect_length(constraints(dset$flights), 2)

  result <- check_constraints(dset)

})

test_that("alternative reference specification formats are equivalent", {

  dset <- qf_dataset(
    flights = nycflights13::flights,
    planes = nycflights13::planes
  )
  constraints(dset, planes) <- list(cstr_primary_key(tailnum))

  dset1 <- dset
  constraints(dset1, flights) <- list(
    cstr_primary_key(c(flight, year, month, day)),
    cstr_foreign_key(tailnum, reference = planes)
  )
  dset2 <- dset
  constraints(dset2, flights) <- list(
    cstr_primary_key(c(flight, year, month, day)),
    cstr_foreign_key(tailnum, reference = planes$tailnum)
  )
  dset3 <- dset
  constraints(dset3, flights) <- list(
    cstr_primary_key(c(flight, year, month, day)),
    cstr_foreign_key(tailnum, reference = planes[tailnum])
  )
  expect_identical(constraints(dset1, flights), constraints(dset2, flights))
  expect_identical(constraints(dset2, flights), constraints(dset3, flights))

})

test_that("table name inference works", {
  dset <- withr::with_package("nycflights13",
    qf_dataset(carriers = airlines, airports)
  )
  expect_equal(names(dset), c("carriers", "airports"))
})
