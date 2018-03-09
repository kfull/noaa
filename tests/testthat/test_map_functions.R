library(testthat)
library(noaa)
library(readr)

# test function eq_create_label
test_that("eq_create_label works correctly", {
  # set working directory
  setwd(system.file("extdata", package = "noaa"))
  # read and clean NOAA data
  data <- readr::read_delim("signif.txt", "\t") %>%
     eq_clean_data() %>%
     dplyr::filter(COUNTRY == "MEXICO", YEAR > 2000)
  # create HTML labels
  html_label <- data %>% eq_create_label(.)
  # test result
  expect_is(html_label, "character")
})

# test function eq_map
test_that("eq_map works correctly", {
  # set working directory
  setwd(system.file("extdata", package = "noaa"))
  # read and clean NOAA data
  data <- readr::read_delim("signif.txt", "\t") %>%
     eq_clean_data() %>%
     dplyr::filter(COUNTRY == "MEXICO", YEAR > 2000)
  # map and annotate NOAA data
  plot <- eq_map(data, annot_col = "DATE")
  # test result
  expect_is(plot, "leaflet")
})

