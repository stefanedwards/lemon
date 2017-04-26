# Files given here are from ggplot2 base.
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
#' @export
#' @keywords internal
waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")


# From ggplot2/R/utilities.r
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}

render_axis <- ggplot2:::render_axis


#' Absolute grob
#'
#' This grob has fixed dimensions and position.
#'
#' It's still experimental
#'
#' Taken directly from ggplot2/R/grob-absolute.r
#'
#' @keywords internal
absoluteGrob <- function(grob, width = NULL, height = NULL,
                         xmin = NULL, ymin = NULL, vp = NULL) {
  
  gTree(
    children = grob,
    width = width, height = height,
    xmin = xmin, ymin = ymin,
    vp = vp, cl = "absoluteGrob"
  )
}

#' Find panels in a gtable
#'
#' These functions help detect the placement of panels in a gtable, if they are
#' named with "panel" in the beginning. \code{find_panel} returns the extend of
#' the panel area, while \code{panel_cols} and \code{panel_rows} returns the
#' columns and rows that contains panels respectively.
#'
#' Taken directly from ggplot2/R/facet-.r
#'
#' @param table A gtable
#'
#' @return A data.frame with some or all of the columns t(op), r(ight),
#' b(ottom), and l(eft)
#'
#' @keywords internal
#' @export
find_panel <- function(table) {
  layout <- table$layout
  panels <- layout[grepl("^panel", layout$name), , drop = FALSE]
  
  data.frame(
    t = min(panels$t),
    r = max(panels$r),
    b = max(panels$b),
    l = min(panels$l)
  )
}
