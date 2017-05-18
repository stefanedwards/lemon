# Files given here are from ggplot2 base.
#' @import ggplot2
#' @import grid
#  @import gridExtra
#' @import gtable
NULL

#' ggproto classes used in lemon!
#'
#' If you are creating a new geom, stat, position, or scale in another package,
#' you'll need to extend from \code{ggplot2::Geom}, \code{ggplot2::Stat},
#' \code{ggplot2::Position}, or \code{ggplot2::Scale}.
#'
#' @seealso \code{\link[ggplot2]{ggproto}}, \code{\link[ggplot2]{ggplot2-ggproto}}
#' @keywords internal
#' @name lemon-ggproto
NULL


#' The waivers are from \file{ggplot2/R/utilities.r}.
#'
#' @seealso \code{\link[ggplot2]{waiver}}
#' @rdname ggplot2-non-exports
#' @keywords internal
waiver <- function() structure(list(), class = "waiver")

#' @param x The object to inquery is a \code{waiver}.
#' @rdname ggplot2-non-exports
#' @keywords internal
is.waive <- function(x) inherits(x, "waiver")

#' @rdname ggplot2-non-exports
#' @keywords internal
"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}

#' @keywords internal
#' @rdname ggplot2-non-exports
#' @name if-not-null
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# From ggplot2/R/coord-.r
render_axis <- function(panel_params, axis, scale, position, theme) {
  if (axis == "primary") {
    guide_axis(panel_params[[paste0(scale, ".major")]], panel_params[[paste0(scale, ".labels")]], position, theme)
  } else if (axis == "secondary" && !is.null(panel_params[[paste0(scale, ".sec.major")]])) {
    guide_axis(panel_params[[paste0(scale, ".sec.major")]], panel_params[[paste0(scale, ".sec.labels")]], position, theme)
  } else {
    zeroGrob()
  }
}


# From ggplot2/R/theme-elements.r
element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- ggplot2::calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }

  grob <- ggplot2::element_grob(el, ...)
  ggname(paste(element, name, sep = "."), grob)
}

#' @import grid
ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

is.zero <- function (x)  {
  is.null(x) || inherits(x, "zeroGrob")
}

# From ggplot2/R/grob-absolute.r
absoluteGrob <- function(grob, width = NULL, height = NULL,
                         xmin = NULL, ymin = NULL, vp = NULL) {

  gTree(
    children = grob,
    width = width, height = height,
    xmin = xmin, ymin = ymin,
    vp = vp, cl = "absoluteGrob"
  )
}
