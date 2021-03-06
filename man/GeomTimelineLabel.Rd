% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_functions.R
\docType{data}
\name{GeomTimelineLabel}
\alias{GeomTimelineLabel}
\title{Construct new Class for Timeline Label Geom}
\format{An object of class \code{GeomTimelineLabel} (inherits from \code{Geom}, \code{ggproto}) of length 6.}
\usage{
GeomTimelineLabel
}
\value{
A new ggproto object for a timeline label geom.
}
\description{
\code{GeomTimelineLabel} constructs a new class for a timeline label geom
adding labels to one or many timelines of earthquakes.
}
\section{Aesthetics}{

\code{x}: Column of the given data frame depicting the x value to which the
labels will be added, usually date. \cr
\code{label}: Column of the given data frame depicting the label which will
be added. \cr
\code{size}: Column of the given data frame depicting to which points of
the timeline labels will be added. \cr
\code{y}: Optional. Column of the given data frame depicting the y value of
the timelines, usually location. \cr
\code{nmax}: Optional. Numeric for the maximum number of labels. \cr
\code{colour}: Optional. Color string for the colour of the vertical line of
labels. \cr
\code{alpha}: Optional. Numeric for the alpha level applied to the vertical
line of the labels. \cr
\code{stroke}: Optional. Stroke of the image. \cr
}

\examples{
# import libraries
library(dplyr)
library(readr)
# set working directory
setwd(system.file("extdata", package = "noaa"))
# read and clean NOAA data
data <- read_delim("signif.txt", "\\t") \%>\%
   eq_clean_data() \%>\%
   dplyr::filter(COUNTRY \%in\% c("USA", "CHINA"), YEAR > 2000)
# plot NOAA data
ggplot2::ggplot(data, ggplot2::aes(DATE,
                                   colour = as.numeric(TOTAL_DEATHS),
                                   y = COUNTRY,
                                   size = as.numeric(EQ_PRIMARY))) +
   geom_timeline() +
   geom_timeline_label(ggplot2::aes(label = data$LOCATION_NAME)) +
   ggplot2::guides(size = ggplot2::guide_legend(title = "Richter Scale")) +
   ggplot2::scale_colour_continuous(name = "Number of Deaths") +
   ggplot2::theme_classic()

}
\keyword{datasets}
