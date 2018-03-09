# resolving notes in cmd check
MONTH <- DAY <- YEAR <- LATITUDE <- LONGITUDE <- LOCATION_NAME <- NULL

#' Clean Location Name of NOAA Data
#'
#' The function \code{eq_location_clean} cleans the location names of
#' a raw NOAA data frame by stripping out the country name and converting the
#' location names to title case.
#'
#' @param data A raw NOAA data frame.
#'
#' @return A NOAA data frame with a cleaned location name column.
#'
#' @examples
#' # import libraries
#' library(dplyr)
#' library(readr)
#' # set working directory
#' setwd(system.file("extdata", package = "noaa"))
#' # read and clean NOAA data
#' data <- read_delim("signif.txt", "\t") %>%
#'    eq_location_clean()
#'
#' @importFrom dplyr mutate %>%
#' @importFrom stringr str_to_title
#'
#' @export
eq_location_clean <- function(data) {
  data <- data %>%
    dplyr::mutate(LOCATION_NAME = sapply(LOCATION_NAME, function(x) {
      stringr::str_to_title(strsplit(x, ":\\s+")[[1]][2])
    }))
}
#' Clean NOAA Data
#'
#' The function \code{eq_clean_data} cleans a raw NOAA data frame. In more
#' detail, it adds a date column, converts latitude and longitude columns to
#' numeric, and cleans the location name column.
#'
#' @param data A raw NOAA data frame.
#'
#' @return A clean NOAA data frame.
#'
#' @examples
#' # import libraries
#' library(dplyr)
#' library(readr)
#' # set working directory
#' setwd(system.file("extdata", package = "noaa"))
#' # read and clean NOAA data
#' data <- read_delim("signif.txt", "\t") %>%
#'    eq_clean_data()
#'
#' @importFrom dplyr mutate mutate_at %>%
#' @importFrom tidyr replace_na
#' @importFrom chron day.of.week
#'
#' @export
eq_clean_data <- function(data) {
  data <- data %>%
    # replace missing months and days
    tidyr::replace_na(replace = list(MONTH = 1, DAY = 1)) %>%
    # convert year, month, and day to date
    dplyr::mutate(DATE = as.Date(chron::day.of.week(MONTH, DAY, YEAR),
                                 origin = "1970-01-01")) %>%
    # convert latitude and longitude to numeric
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE)) %>%
    # clean location names
    eq_location_clean()
}
