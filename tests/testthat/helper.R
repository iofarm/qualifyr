example_dataset <- function(constrained = TRUE) {
  dset <- qf_dataset(
    flights  = nycflights13::flights,
    planes   = nycflights13::planes,
    airlines = nycflights13::airlines
  )
  if (constrained) {
    constraints(dset$airlines) <- list(
      cstr_primary_key(carrier),
      cstr_not_missing(name)
    )
    constraints(dset$planes) <- list(
      cstr_primary_key(tailnum)
    )
    constraints(dset$flights) <- list(
      cstr_primary_key(c(year, month, day, carrier, flight)),
      cstr_foreign_key(carrier, airlines)
    )
  }
  dset
}
