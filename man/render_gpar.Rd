% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grob_utils.r
\name{render_gpar}
\alias{render_gpar}
\title{Render a ggplot2 grob or retrieve its gpar object.}
\usage{
render_gpar(theme, element, ...)
}
\arguments{
\item{theme}{A ggplot2 \link[ggplot2]{theme}}

\item{element}{The name of an element in the theme, e.g. "axis.text".}

\item{...}{Additional arguments sent to grobs (e.g. \code{x} or \code{y}).}
}
\value{
A \code{\link[grid]{grid.grob}} or \code{\link[grid]{gpar}} object.
}
\description{
Helps add the ggplot2-theme's look-and-feel to \code{grid}'s grob objects.
\code{render_gpar} returns a \code{\link[grid]{gpar}}-object,
\code{element_render} returns a \code{\link[grid]{grid.grob}}-object.
}
\seealso{
\code{\link[ggplot2]{theme}}
}
\keyword{internal}
