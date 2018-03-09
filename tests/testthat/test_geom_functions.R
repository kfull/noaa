library(testthat)
library(noaa)
library(readr)

# test function geom_timeline
test_that("geom_timeline works correctly", {
  # set working directory
  setwd(system.file("extdata", package = "noaa"))
  # read and clean NOAA data
  data <- read_delim("signif.txt", "\t") %>%
     eq_clean_data() %>%
     dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000)
  # plot NOAA data
  plot <- ggplot2::ggplot(data, ggplot2::aes(DATE,
                                     colour = as.numeric(TOTAL_DEATHS),
                                     y = COUNTRY,
                                     size = as.numeric(EQ_PRIMARY))) +
     geom_timeline() +
     ggplot2::guides(size = ggplot2::guide_legend(title = "Richter Scale")) +
     ggplot2::scale_colour_continuous(name = "Number of Deaths") +
     ggplot2::theme_classic()
  # test result
  expect_is(plot, "ggplot")
})

# test function geom_timeline_label
test_that("geom_timeline_label works correctly", {
  # set working directory
  setwd(system.file("extdata", package = "noaa"))
  # read and clean NOAA data
  data <- read_delim("signif.txt", "\t") %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000)
  # plot NOAA data
  plot <- ggplot2::ggplot(data, ggplot2::aes(DATE,
                                             colour = as.numeric(TOTAL_DEATHS),
                                             y = COUNTRY,
                                             size = as.numeric(EQ_PRIMARY))) +
    geom_timeline() +
    geom_timeline_label(ggplot2::aes(label = data$LOCATION_NAME)) +
    ggplot2::guides(size = ggplot2::guide_legend(title = "Richter Scale")) +
    ggplot2::scale_colour_continuous(name = "Number of Deaths") +
    ggplot2::theme_classic()
  # test result
  expect_is(plot, "ggplot")
})

