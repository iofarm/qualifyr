test_that("constraint checking works", { expect_no_error({

  dset <- qf_dataset(
    flights = nycflights13::flights,
    planes = nycflights13::planes
  )

  constraints(dset, planes) <- list(
    cstr_primary_key(tailnum)
  )
  constraints(dset, flights) <- list(
    cstr_primary_key(c(flight, year, month, day)),
    cstr_foreign_key(tailnum, tailnum %from% planes)
  )

  result <- check_constraints(dset)

}) })
