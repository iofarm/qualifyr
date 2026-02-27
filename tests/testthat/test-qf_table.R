test_that("constraint selection works", {
  dset <- example_dataset()

  # selecting constraints by type or by type and columns works
  expect_equal(
    constraint_index(dset$flights, class = "cstr_foreign_key"),
    2
  )
  expect_identical(
    get_primary_key(dset$flights),
    constraints(dset$flights)[[1]]
  )
  expect_identical(
    get_primary_key(dset$flights, c(year, month, day, carrier, flight)),
    constraints(dset$flights)[[1]]
  )

  # invalid selections fail
  expect_error(regex = "does not match any constraints", {
    get_foreign_key(dset$airlines)
  })
  constraints(dset$airlines) <- c(
    constraints(dset$airlines),
    list(cstr_not_missing(carrier))
  )
  expect_error(regex = "matches multiple constraints", {
    get_not_missing(dset$airlines)
  })

  # replacing constraint is equivalent to setting constraint
  old_primary_key <- get_primary_key(dset$flights)
  get_primary_key(dset$flights) <-
    cstr_primary_key(c(year, month, day, carrier, flight))
  new_primary_key <- get_primary_key(dset$flights)
  expect_identical(old_primary_key, new_primary_key)

  old_foreign_key <- get_foreign_key(dset$flights)
  get_foreign_key(dset$flights) <-
    cstr_foreign_key(carrier, airlines)
  new_foreign_key <- get_foreign_key(dset$flights)
  expect_identical(old_foreign_key, new_foreign_key)

  # modifying constraints works # but don't do this irl
  get_primary_key(dset$flights)$cols <- c("year", "month", "day", "flight")
  expect_identical(
    get_primary_key(dset$flights)$cols,
    c("year", "month", "day", "flight")
  )
})

test_that("coersion works", {
  expect_identical(
    class(as_qf_table(mtcars)),
    c("qf_table", "data.frame")
  )
  expect_error(regex = "cannot coerce",
    as_qf_table(table)
  )
})

test_that("print() output is correct", {
  dset <- example_dataset()
  expect_snapshot_output(print(dset$planes))
})

test_that("argument type checking works", {
  dset <- example_dataset()
  expect_error(regexp = "'x' must be a qf_table, not NULL", {
    constraints(dset$planez) <-
      cstr_primary_key(tailnum)
  })
})

# test_that("renaming columns does not break constraints", {
#   dset <- example_dataset()
#   dset$planes <- dplyr::rename(dset$planes, tailnumber = tailnum)
#   expect_no_error(
#     validate_qf_constraint(get_foreign_key(dset$flights), dset$flights)
#   )
# })
