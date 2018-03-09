#' Map and Annotate Earthquakes
#'
#' The function \code{eq_map} maps the epicenters of earthquakes and annotates
#' each point with a pop up window.
#'
#' @param data A NOAA data frame with the columns LONGITUDE and LATITUDE of
#' the earthquakes.
#' @param annot_col The column of the data frame depicting the annotation of
#' the earthquakes.
#'
#' @return The function outputs a map with epicenters of earthquakes as well
#' as their annotations to the current graphics device.
#'
#' @examples
#' # import libraries
#' library(dplyr)
#' library(readr)
#' # set working directory
#' setwd(system.file("extdata", package = "noaa"))
#' # read and clean NOAA data
#' data <- readr::read_delim("signif.txt", "\t") %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO", YEAR > 2000)
#' # map and annotate NOAA data
#' eq_map(data, annot_col = "DATE")
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom dplyr %>%
#'
#' @export
eq_map = function(data, annot_col) {
  annotation = data[[annot_col]]
  mapping = leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      data = data,
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      popup = ~ annotation,
      weight = 1
    )
  return(mapping)
}


#' Create HTML Label for Earthquakes
#'
#' The function \code{eq_create_label} creates HTML labels for earthquakes
#' that can be used as annotation text in function \code{eq_map}.
#'
#' @param data A NOAA data frame.
#' @param location The column of the data frame depicting the location of
#' earthquakes.
#' @param magnitude The column of the data frame depicting the magnitude of
#' earthquakes.
#' @param total_deaths The column of the data frame depicitn the number of
#' total deaths of earthquakes.
#'
#' @return A HTML label containing location, magnitude, and total deaths
#' information of earthquakes.
#'
#' @examples
#' # import libraries
#' library(dplyr)
#' library(readr)
#' # set working directory
#' setwd(system.file("extdata", package = "noaa"))
#' # read and clean NOAA data
#' data <- readr::read_delim("signif.txt", "\t") %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO", YEAR > 2000)
#' # create HTML labels
#' data <- data %>%
#'    dplyr::mutate(html_label = eq_create_label(.))
#' # map and annotate NOAA data
#' eq_map(data, annot_col = "html_label")
#'
#' @export
eq_create_label = function(data,
                           location = "LOCATION_NAME",
                           magnitude = "EQ_PRIMARY",
                           total_deaths = "TOTAL_DEATHS") {
  html_string_vector = c()
  for (i in 1:nrow(data)) {
    html_string = ''
    if (!is.na(data[i, location])) {
      html_string = paste(html_string,
                          "<b>Location:</b>",
                          data[i, location],
                          "<br/>")
    }
    if (!is.na(data[i, magnitude])) {
      html_string = paste(html_string,
                          "<b>Magnitude:</b>",
                          data[i, magnitude],
                          "<br/>")
    }
    if (!is.na(data[i, total_deaths])) {
      html_string = paste(html_string,
                          "<b>Total deaths:</b>",
                          data[i, total_deaths])
    }
    html_string_vector = c(html_string_vector, html_string)
  }
  html_string_vector
}
