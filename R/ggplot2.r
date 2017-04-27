# Files given here are from ggplot2 base.
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import gtable
NULL

#' Base ggproto classes for ggplot2
#'
#' If you are creating a new geom, stat, position, or scale in another package,
#' you'll need to extend from \code{ggplot2::Geom}, \code{ggplot2::Stat},
#' \code{ggplot2::Position}, or \code{ggplot2::Scale}.
#'
#
#' @seealso \code{\link[ggplot2]{ggproto}}
#' @keywords internal
#' @name ggplot2-ggproto
NULL

#' A waiver object.
#'
#' A waiver is a "flag" object, similar to \code{NULL}, that indicates the
#' calling function should just use the default value.  It is used in certain
#' functions to distinguish between displaying nothing (\code{NULL}) and
#' displaying a default value calculated elsewhere (\code{waiver()})
#'
#' Code taken from \file{ggplot2/R/utilities.r}.
#'
#' @references ggplot2
#' @rdname waiver
#' @keywords internal
waiver <- function() structure(list(), class = "waiver")

#' @param x The object to inquery is a \code{waiver}.
#' @rdname waiver
#' @keywords internal
is.waive <- function(x) inherits(x, "waiver")


#' @rdname waiver
#' @keywords internal
"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}


#' Return if not null
#' 
#' Returns second argument if first argument is \code{NULL}.
#' 
#' Code taken from \file{ggplot2/R/utilities.r}.
#' @keywords internal
#' @rdname if-not-null
#' @name if-not-null
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
