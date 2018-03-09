#' Construct new Class for Timeline Geom
#'
#' \code{GeomTimeline} constructs a new class for a timeline geom plotting one
#' or multiple timelines of earthquakes.
#'
#' @inheritParams ggplot2::ggproto
#'
#' @return A new ggproto object for a timeline geom.
#'
#' @section Aesthetics:
#' \code{x}: The column of the given data frame depicting the x values of the
#' timeline geom, usually date. \cr
#' \code{y}: Optional. The column of the given data frame depicting the y
#' values of the timeline geom, usually location. \cr
#' \code{size}: Optional. The column of the given data frame depicting the size
#' of each point in the timeline geom. \cr
#' \code{colour}: Optional.  The column of the given data frame depicting the
#' color of each point in the timeline geom. \cr
#' \code{fill}: Optional. A color string depicting the default fill of each
#' point in the timeline geom. \cr
#' \code{shape}: Optional. The column of the given data frame depicting the
#' shape of each point in the timeline geom. \cr
#' \code{alpha}: Optional. The column of the given data frame depicting the
#' alpha value of each point in the timeline geom. \cr
#' \code{stroke}: Optional. The stroke of the timeline geom. \cr
#'
#' @examples
#' # import libraries
#' library(dplyr)
#' library(readr)
#' # set working directory
#' setwd(system.file("extdata", package = "noaa"))
#' # read and clean NOAA data
#' data <- read_delim("signif.txt", "\t") %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000)
#' # plot NOAA data
#' ggplot2::ggplot(data, ggplot2::aes(DATE,
#'                                    colour = as.numeric(TOTAL_DEATHS),
#'                                    y = COUNTRY,
#'                                    size = as.numeric(EQ_PRIMARY))) +
#'    geom_timeline() +
#'    ggplot2::guides(size = ggplot2::guide_legend(title = "Richter Scale")) +
#'    ggplot2::scale_colour_continuous(name = "Number of Deaths") +
#'    ggplot2::theme_classic()
#'
#' @importFrom scales alpha
#' @importFrom grid unit gList linesGrob pointsGrob gpar
#' @importFrom ggplot2 .pt ggproto aes draw_key_point Geom
GeomTimeline = ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = "x",
  default_aes = ggplot2::aes(
    shape = 21,
    y = 0.5,
    fill = "grey10",
    alpha = 0.5,
    colour = "black",
    size = 6,
    stroke = 0
  ),
  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_scales, coord) {
    # transform parameters to panel coordinates
    coords <-
      coord$transform(data, panel_scales)

    # create list of grobs to draw plot
    main_list = grid::gList()

    # create straight line
    line_range = c(min(coords$x), max(coords$x))

    # loop over countries
    for (elem in unique(coords$y)) {
      # draw straight line
      lG = grid::linesGrob(line_range,
                           elem,
                           gp = grid::gpar(alpha = coords$alpha /
                                             2))
      # add grob to the main list
      main_list = grid::gList(main_list, lG)
    }

    # create points geom to describe each data point
    coords$size = coords$size / 2.0
    pG = grid::pointsGrob(
      coords$x,
      coords$y,
      pch = coords$shape,
      # shape of the points
      size = grid::unit(coords$size, "char"),
      # size of each dot
      gp = grid::gpar(
        col = scales::alpha(coords$colour, coords$alpha),
        fill = scales::alpha(coords$colour, coords$alpha),
        alpha = coords$alpha,
        fontsize = coords$size * ggplot2::.pt
      )
    )

    # add points grob to the main list
    grid::gList(pG, main_list)
  }
)

#' Plot Timeline Geom
#'
#' The function \code{geom_timeline} plots a timeline geom showing one or
#' multiple timelines of earthquakes.
#'
#' @inheritParams ggplot2::layer
#' @param na.rm Boolean indicating wheather to remove NA values.
#' @param ... Additional optional parameters.
#'
#' @return The function \code{geom_timeline} plots a timeline geom.
#'
#' @section Aesthetics:
#' \code{x}: The column of the given data frame depicting the x values of the
#' timeline geom, usually date. \cr
#' \code{y}: Optional. The column of the given data frame depicting the y
#' values of the timeline geom, usually location. \cr
#' \code{size}: Optional. The column of the given data frame depicting the size
#' of each point in the timeline geom. \cr
#' \code{colour}: Optional.  The column of the given data frame depicting the
#' color of each point in the timeline geom. \cr
#' \code{fill}: Optional. A color string depicting the default fill of each
#' point in the timeline geom. \cr
#' \code{shape}: Optional. The column of the given data frame depicting the
#' shape of each point in the timeline geom. \cr
#' \code{alpha}: Optional. The column of the given data frame depicting the
#' alpha value of each point in the timeline geom. \cr
#' \code{stroke}: Optional. The stroke of the image. \cr
#'
#' @examples
#' # import libraries
#' library(dplyr)
#' library(readr)
#' # set working directory
#' setwd(system.file("extdata", package = "noaa"))
#' # read and clean NOAA data
#' data <- read_delim("signif.txt", "\t") %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000)
#' # plot NOAA data
#' ggplot2::ggplot(data, ggplot2::aes(DATE,
#'                                    colour = as.numeric(TOTAL_DEATHS),
#'                                    y = COUNTRY,
#'                                    size = as.numeric(EQ_PRIMARY))) +
#'    geom_timeline() +
#'    ggplot2::guides(size = ggplot2::guide_legend(title = "Richter Scale")) +
#'    ggplot2::scale_colour_continuous(name = "Number of Deaths") +
#'    ggplot2::theme_classic()
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline = function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Construct new Class for Timeline Label Geom
#'
#' \code{GeomTimelineLabel} constructs a new class for a timeline label geom
#' adding labels to one or many timelines of earthquakes.
#'
#' @inheritParams ggplot2::ggproto
#'
#' @return A new ggproto object for a timeline label geom.
#'
#' @section Aesthetics:
#' \code{x}: Column of the given data frame depicting the x value to which the
#' labels will be added, usually date. \cr
#' \code{label}: Column of the given data frame depicting the label which will
#' be added. \cr
#' \code{size}: Column of the given data frame depicting to which points of
#' the timeline labels will be added. \cr
#' \code{y}: Optional. Column of the given data frame depicting the y value of
#' the timelines, usually location. \cr
#' \code{nmax}: Optional. Numeric for the maximum number of labels. \cr
#' \code{colour}: Optional. Color string for the colour of the vertical line of
#' labels. \cr
#' \code{alpha}: Optional. Numeric for the alpha level applied to the vertical
#' line of the labels. \cr
#' \code{stroke}: Optional. Stroke of the image. \cr
#'
#' @examples
#' # import libraries
#' library(dplyr)
#' library(readr)
#' # set working directory
#' setwd(system.file("extdata", package = "noaa"))
#' # read and clean NOAA data
#' data <- read_delim("signif.txt", "\t") %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000)
#' # plot NOAA data
#' ggplot2::ggplot(data, ggplot2::aes(DATE,
#'                                    colour = as.numeric(TOTAL_DEATHS),
#'                                    y = COUNTRY,
#'                                    size = as.numeric(EQ_PRIMARY))) +
#'    geom_timeline() +
#'    geom_timeline_label(ggplot2::aes(label = data$LOCATION_NAME)) +
#'    ggplot2::guides(size = ggplot2::guide_legend(title = "Richter Scale")) +
#'    ggplot2::scale_colour_continuous(name = "Number of Deaths") +
#'    ggplot2::theme_classic()
#'
#' @importFrom scales alpha
#' @importFrom grid unit gList linesGrob textGrob gpar
#' @importFrom ggplot2 .pt ggproto aes draw_key_abline Geom
#' @importFrom dplyr arrange
#' @importFrom utils head
GeomTimelineLabel = ggplot2::ggproto(
  "GeomTimelineLabel",
  ggplot2::Geom,
  required_aes = c("x", "label", "size"),
  default_aes = ggplot2::aes(
    y = 0.5,
    nmax = 5,
    stroke = 0,
    colour = NA,
    alpha = 0.5
  ),
  draw_key = ggplot2::draw_key_abline,
  setup_data = function(data, params) {
    line_length = length(unique(data$y))
    if (line_length < 1) {
      line_length = 1
    }

    # return the variable as part of the data
    data$line_length = line_length
    data
  },

  # loop over y values
  draw_group = function(data, panel_scales, coord) {
    # transform parameters to panel coordinates
    coords <-
      coord$transform(data, panel_scales)

    # create list of grobs to draw plot
    main_list = grid::gList()

    # convert data to data frame
    data_point = data.frame(
      x = coords$x,
      y = coords$y,
      size = coords$size,
      label = coords$label
    )

    data_point = data_point %>%
      dplyr::arrange(desc(size)) %>%
      utils::head(coords$nmax[1])

    # loop over top labels
    for (i in 1:nrow(data_point)) {
      # create geom line
      lG = grid::linesGrob(
        rep(data_point[i, "x"], 2),
        # make vertical line
        c(data_point[i, "y"],
          data_point[i, "y"] + 0.15 /
            coords$line_length),
        # adjust  height based on degrees of y
        gp = grid::gpar(col = scales::alpha(coords$colour, coords$alpha/2))
      )

      # create geom text
      tG = grid::textGrob(
        data_point[i, "label"],
        data_point[i, "x"],
        data_point[i, "y"] + 0.18 / coords$line_length,
        rot = 45,
        just = c("left", "bottom"),
        gp = grid::gpar(
          col = "grey20",
          fontsize = 12,
          fontface = "plain"
        ) # text aethetics
      )

      # add both grobs to main grob list
      main_list = grid::gList(main_list, lG, tG)
    }

    main_list
  }
)


#' Plot Timeline Label Geom
#'
#' The function \code{geom_timeline} plots a timeline label geom labels to one
#' or many timelines of earthquakes.
#'
#' @inheritParams ggplot2::layer
#' @param na.rm Boolean indicating wheather to remove NA values.
#' @param ... Additional optional parameters.
#'
#' @return The function \code{geom_timeline_label} plots a timeline label geom.
#'
#' @section Aesthetics:
#' \code{x}: Column of the given data frame depicting the x value to which the
#' labels will be added, usually date. \cr
#' \code{label}: Column of the given data frame depicting the label which will
#' be added. \cr
#' \code{size}: Column of the given data frame depicting to which points of
#' the timeline labels will be added. \cr
#' \code{y}: Optional. Column of the given data frame depicting the y value of
#' the timelines, usually location. \cr
#' \code{nmax}: Optional. Numeric for the maximum number of labels. \cr
#' \code{colour}: Optional. Color string for the colour of the vertical line of
#' labels. \cr
#' \code{alpha}: Optional. Numeric for the alpha level applied to the vertical
#' line of the labels. \cr
#' \code{stroke}: Optional. Stroke of the image. \cr
#'
#' @examples
#' # import libraries
#' library(dplyr)
#' library(readr)
#' # set working directory
#' setwd(system.file("extdata", package = "noaa"))
#' # read and clean NOAA data
#' data <- read_delim("signif.txt", "\t") %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000)
#' # plot NOAA data
#' ggplot2::ggplot(data, ggplot2::aes(DATE,
#'                                    colour = as.numeric(TOTAL_DEATHS),
#'                                    y = COUNTRY,
#'                                    size = as.numeric(EQ_PRIMARY))) +
#'    geom_timeline() +
#'    geom_timeline_label(ggplot2::aes(label = data$LOCATION_NAME)) +
#'    ggplot2::guides(size = ggplot2::guide_legend(title = "Richter Scale")) +
#'    ggplot2::scale_colour_continuous(name = "Number of Deaths") +
#'    ggplot2::theme_classic()
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline_label = function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               na.rm = TRUE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
