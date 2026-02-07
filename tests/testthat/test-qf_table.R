test_that("constraint selection works", {
  dset <- withr::with_package("nycflights13",
    qf_dataset(airlines, flights)
  )
  constraints(dset$airlines) <- list(
    cstr_primary_key(carrier)
  )
  constraints(dset$flights) <- list(
    cstr_primary_key(c(year, month, day, carrier, flight)),
    cstr_foreign_key(carrier, airlines)
  )

  # selecting constraints by type works
  expect_equal(
    constraint_index(dset$flights, class = "cstr_foreign_key"),
    2
  )
  expect_identical(
    primary_key(dset$flights),
    constraints(dset$flights)[[1]]
  )

  # replacing constraint is equivalent to setting constraint
  old_primary_key <- primary_key(dset$flights)
  primary_key(dset$flights) <-
    cstr_primary_key(c(year, month, day, carrier, flight))
  new_primary_key <- primary_key(dset$flights)
  expect_identical(old_primary_key, new_primary_key)

  # modifying constraints works
  primary_key(dset$flights)$cols <- c("year", "month", "day", "flight")
  expect_identical(
    primary_key(dset$flights)$cols,
    c("year", "month", "day", "flight")
  )
})
