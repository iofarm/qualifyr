test_that("exceptions work", {
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

  exceptions(primary_key(dset$flights)) <- list(
    except_where(carrier %in% "WN", flight %in% 2269),
    except_where(carrier %in% "UA", flight %in% c(207, 236, 258, 635))
  )
  result <- check_constraints(dset)$flights[[1]]
  expect_false(result$satisfied)
  expect_true(result$handled)
})
