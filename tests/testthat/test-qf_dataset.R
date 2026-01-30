test_that("constraint checking works", {

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
    cstr_foreign_key(tailnum, tailnum %from% planes)
  )
  expect_length(constraints(dset$flights), 2)

  result <- check_constraints(dset)

})
