test_that("exceptions work", {
  dset <- example_dataset()
  exceptions(get_primary_key(dset$flights)) <-
    except_where(carrier %in% "WN", flight %in% 2269) |
    except_where(carrier %in% "UA", flight %in% c(207, 236, 258, 635))
  result <- check_constraints(dset)$flights[[1]]
  expect_false(satisfied(result))
  expect_true(handled(result))
})
