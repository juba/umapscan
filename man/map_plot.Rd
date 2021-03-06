% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/umapscan.R
\name{map_plot}
\alias{map_plot}
\title{Display an umapscan clustering as a leaflet map.}
\usage{
map_plot(us, point_labels = NULL, labels = TRUE)
}
\arguments{
\item{us}{umapscan object to be plotted}

\item{point_labels}{a character vector of point labels displayed
in a popup on hover. Can contain HTML.}

\item{labels}{if TRUE, us labels instead of identifiers to identify clusters}
}
\description{
Display an umapscan clustering as a leaflet map.
}
