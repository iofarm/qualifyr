test_that("exceptions work", {
  dset <- example_dataset()
  exceptions(primary_key(dset$flights)) <- list(
    except_where(carrier %in% "WN", flight %in% 2269),
    except_where(carrier %in% "UA", flight %in% c(207, 236, 258, 635))
  )
  result <- check_constraints(dset)$flights[[1]]
  expect_false(result$satisfied)
  expect_true(result$handled)
})
