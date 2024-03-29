% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-siderange.r
\name{geom_siderange}
\alias{geom_siderange}
\title{Display range of data in side of plot}
\usage{
geom_siderange(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  distance = 3,
  arrow = NULL,
  lineend = "butt",
  sides = "bl",
  start = NA,
  end = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
or \code{\link[ggplot2]{aes_}}.}

\item{data}{The data to be displayed in this layer.}

\item{stat}{The statistical transformation to use on the data for this layer,
as a string.}

\item{position}{Position adjustment, either as a string, or the result of a
call to a position adjustment function
(e.g. \code{\link[ggplot2]{position_jitter}}).
Both lines and points gets the same adjustment
(\emph{this} is where the function excels over \code{geom_point() + geom_line()}).}

\item{...}{other arguments passed on to \code{\link[ggplot2]{layer}}.}

\item{distance}{Distance between edge of panel and lines, and distance
between lines, in multiples of line widths, see description.}

\item{arrow}{Arrow specification, as created by \code{\link[grid]{arrow}}.}

\item{lineend}{Line end style (round, butt, square).}

\item{sides}{Character including \strong{t}op, \strong{r}ight, \strong{b}ottom, and/or \strong{l}eft,
indicating which side to project data onto.}

\item{start, end}{Adds a symbol to either end of the siderange.
\code{start} corresponds to minimal value, \code{end} to maximal value.}

\item{na.rm}{If \code{FALSE} (default), missing values are removed with a warning.
If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{Logical. Should this layer be included in the legends?
\code{NA} (default), includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetic, rather
than combining with them. This is most useful for helper functions that
define both data and aesthetics and shouldn't inherit behaviour from the
default plot specification.}
}
\description{
Projects data onto horizontal or vertical edge of panels.
}
\details{
The \code{geom_siderange} projects the data displayed in the panel onto the
sides, using the same aesthetics. It has the added capability of potting a
symbol  at either end
of the line, and lines are offset from the edge and each other.

To display a symbol, specify an integer for either \code{start} or \code{end}.
See the list for \code{pch} in \code{\link[graphics]{points}} for values to use.
The argumetns \code{start} and \code{end} also accepts a list object with
named entries \code{pch}, \code{alpha}, \code{stroke}, and \code{fill}, which
correspond to the usual aesthetics, as well as a special named entry,
\code{sizer} (note the extra 'r').
This last entry is a multiplier for enlarging the symbol relative to the
linewidth, as the aesthetic \code{size} affects both linewidth and symbol size.

The distance between the panel's edge and sideranges are specified by
the argument \code{distance}. If a symbol is specified, the linewidth is
further expanded to cover the width of the symbol (including \code{sizer}).
}
\section{Aesthetics}{

The geom understands the following aesthetics (required are in bold):
\itemize{
  \item \strong{x}
  \item \strong{y}
  \item alpha
  \item colour
  \item fill (if a symbol is applied with \code{start} or \code{end}
  \item group
  \item linetype
  \item size
  \item stroke
}
}

\examples{
library(ggplot2)

x <- rnorm(25)
df <- data.frame(x=x, y=x+rnorm(25, sd=0.2), 
                 a=sample(c('horse','goat'), 25, replace=TRUE), 
                 stringsAsFactors = FALSE)
df$y <- with(df, ifelse(y > 1 & a=='horse', 1, y))
(p <- ggplot(df, aes(x=x, y=y, colour=a)) + geom_point(shape=1))

p + geom_siderange(start=19)

# Capping the sideranges with different symbols:
p + geom_siderange(start=19, end=22, fill='black', sides='b') + geom_siderange(sides='tl')

# It also works with facets

p <- ggplot(mpg, aes(displ, hwy, colour=fl)) +
  geom_point() +
  facet_wrap(~class, nrow = 4)

p + geom_siderange()

}
\seealso{
\code{\link[ggplot2]{geom_rug}}
}
