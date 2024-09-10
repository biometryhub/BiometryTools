test_that("Getting sunrise/sunset times works", {
  date_ <- lubridate::as_datetime("2020-01-01", tz = "Australia/Adelaide")
  sunset <- sunset_time(date_, -35.69167, 136.9650)

  expect_true(lubridate::is.POSIXct(sunset))
  expect_equal(toString(as.Date(sunset, "Australia/Adelaide")), "2020-01-01")

  sunrise <- sunrise_time(date_, -35.69167, 136.9650)

  expect_true(lubridate::is.POSIXct(sunrise))
  expect_equal(toString(as.Date(sunrise, "Australia/Adelaide")), "2020-01-01")
})


test_that("Invalid parameters throw errors", {
  date_ <- lubridate::as_datetime("2020-01-01", tz = "Australia/Adelaide")

  expect_error(sunset_time("2020-01-01", -35.69167, 136.9650), "`datetime` must be a datetime object.")
  expect_error(sunset_time(date_, -90.1, 136.9650), "`latitude` must be inclusively between -90 and 90.")
  expect_error(sunset_time(date_, "90", 136.9650), "`latitude` must be inclusively between -90 and 90.")
  expect_error(sunset_time(date_, -90, 181), "`longitude` must be inclusively between -180 and 180.")
  expect_error(sunset_time(date_, -90, "179"), "`longitude` must be inclusively between -180 and 180.")
})
