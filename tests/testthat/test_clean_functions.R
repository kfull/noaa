library(testthat)
library(noaa)
library(readr)

# test function eq_location_clean
test_that("eq_location_clean works correctly", {
  # set working directory
  setwd(system.file("extdata", package = "noaa"))
  # read and clean NOAA data
  data <- read_delim("signif.txt", "\t") %>%
    eq_location_clean()
  # test result
  expect_is(data$LOCATION_NAME, "character")
})

# test function eq_clean_data
test_that("eq_clean_data works correctly", {
  # set working directory
  setwd(system.file("extdata", package = "noaa"))
  # read and clean NOAA data
  data <- read_delim("signif.txt", "\t") %>%
    eq_clean_data()
  # test result
  expect_is(data$DATE,"Date")
  expect_is(data$LATITUDE,"numeric")
  expect_is(data$LONGITUDE,"numeric")
})
