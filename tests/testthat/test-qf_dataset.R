test_that("table name inference works", {
  dset <- withr::with_package("nycflights13",
    qf_dataset(carriers = airlines, airports)
  )
  expect_equal(names(dset), c("carriers", "airports"))
})
