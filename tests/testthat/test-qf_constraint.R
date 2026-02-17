test_that("setting constraints works", {
  dset <- example_dataset(constrained = FALSE)

  constraints(dset$planes) <-
    cstr_primary_key(tailnum)
  expect_identical(
    constraints(dset$planes),
    new_qf_constraint_list(list(
      structure(
        list(cols = "tailnum"),
        class = c("cstr_primary_key", "cstr_unique_key", "qf_constraint")
      )
    ))
  )

  constraints(dset$flights) <-
    cstr_primary_key(c(flight, year, month, day)) &
    cstr_foreign_key(tailnum, reference = planes)
  expect_identical(
    constraints(dset$flights),
    new_qf_constraint_list(list(
      structure(
        list(cols = c("flight", "year", "month", "day")),
        class = c("cstr_primary_key", "cstr_unique_key", "qf_constraint")
      ),
      structure(
        list(cols = "tailnum", ref_table = "planes", ref_cols = "tailnum"),
        class = c("cstr_foreign_key", "qf_constraint")
      )
    ))
  )

  # Foreign key referencing own table:
  enneagram <- as_qf_table(data.frame(
    number         = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    disintegration = c(4, 8, 9, 2, 7, 3, 1, 5, 6),
    integration    = c(7, 4, 6, 1, 8, 9, 5, 2, 3)
  ))
  constraints(enneagram) <-
    cstr_primary_key(number) &
    cstr_foreign_key(disintegration, .self$number) &
    cstr_foreign_key(integration, .self$number)
  expect_identical(
    constraints(enneagram),
    new_qf_constraint_list(list(
      structure(
        list(cols = "number"),
        class = c("cstr_primary_key", "cstr_unique_key", "qf_constraint")
      ),
      structure(
        list(cols = "disintegration", ref_table = ".self", ref_cols = "number"),
        class = c("cstr_foreign_key", "qf_constraint")
      ),
      structure(
        list(cols = "integration", ref_table = ".self", ref_cols = "number"),
        class = c("cstr_foreign_key", "qf_constraint")
      )
    ))
  )
  expect_error(regex = "no information on other tables in the dataset",  {
    constraints(enneagram) <- list(
      cstr_primary_key(number),
      cstr_foreign_key(disintegration, .self$number),
      cstr_foreign_key(integration, .self$number),
      cstr_foreign_key(integration, some_other_table)
    )
  })

})

test_that("alternative reference specification formats are equivalent", {
  dset <- example_dataset(constrained = FALSE)
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
  dset <- example_dataset()

  # Case 1: Primary key on 'flights' is invalid because of duplicate keys;
  #   otherwise OK
  result <- check_constraints(dset)
  expect_true(satisfied(result$airlines[[1]]))
  expect_true(satisfied(result$airlines[[2]]))
  expect_false(satisfied(result$flights[[1]]))
  expect_true(satisfied(result$flights[[2]]))

  expect_snapshot_output(print(result))

  # Case 2: Primary key and not-missing constraint on 'airlines' is invalid
  #   because of missing values for Delta Airlines; Foreign key on 'flights' is
  #   invalid because it references this primary key.
  dset2 <- dset
  dset2$airlines[dset$airlines$carrier == "DL", ] <- NA
  result2 <- check_constraints(dset2)
  expect_false(satisfied(result2$airlines[[1]]))
  expect_false(satisfied(result2$airlines[[2]]))
  expect_false(satisfied(result2$flights[[1]]))
  expect_false(satisfied(result2$flights[[2]]))

  # Foreign key referencing own table:
  enneagram <- as_qf_table(data.frame(
    number         = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    disintegration = c(4, 8, 9, 2, 7, 3, 1, 5, 6),
    integration    = c(7, 4, 6, 1, 8, 9, 5, 2, 3)
  ))
  constraints(enneagram) <- list(
    cstr_primary_key(number),
    cstr_foreign_key(disintegration, .self$number),
    cstr_foreign_key(integration, .self$number)
  )
  result9 <- check_constraints(enneagram)
  expect_true(satisfied(result9[[2]]))
  expect_true(satisfied(result9[[3]]))

})

test_that("constraint validators work", {
  dset <- example_dataset(constrained = FALSE)

  constraints(dset$airlines) <- list(
    cstr_not_missing(name)
  )
  expect_error(regexp = "includes non-existent columns", {
    get_not_missing(dset$airlines)$cols <- "nombre"
  })
  expect_error(regexp = "does not reference a unique key", {
    constraints(dset$flights) <- list(
      cstr_primary_key(c(year, month, day, carrier, flight)),
      cstr_foreign_key(carrier, airlines)
    )
  })
})

test_that("apply_to_each() works", {
  dset <- example_dataset(constrained = FALSE)
  constraints(dset$planes) <-
    cstr_primary_key(tailnum) &
    apply_to_each(cstr_not_missing, year, type, manufacturer, model)
  expect_true(
    purrr::every(constraints(dset$planes)[-1], inherits, "cstr_not_missing")
  )
  expect_equal(
    purrr::map_chr(constraints(dset$planes)[-1], "cols") |> unname(),
    c("year", "type", "manufacturer", "model")
  )
})
