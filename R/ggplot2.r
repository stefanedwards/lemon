# Files given here are from ggplot2 base.
#' @import ggplot2
#' @import grid
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
#' @rawNamespace if(getRversion() < "4.4")  export("%||%")
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#  From ggplot2/R/utilities-grid.r
#' @import grid
ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

is.zero <- function (x)  {
  is.null(x) || inherits(x, "zeroGrob")
}

# From ggplot2/R/coord-cartesian-.r
panel_guide_label <- function(guides, position, default_label) {
  if (inherits(guides, "Guides")) {
    pair <- guides$get_position(position)
    pair$params$title %|W|% default_label
  } else {
    guide <- guide_for_position(guides, position) %||% guide_none(title = waiver())
    guide$title %|W|% default_label
  }
}

panel_guides_grob <- function(guides, position, theme, labels = NULL) {
  if (inherits(guides, "Guides")) {
    pair <- guides$get_position(position)
    pair$params$draw_label <- labels %||% NULL
    pair$guide$draw(theme, params = pair$params)
  } else {
    guide <- guide_for_position(guides, position) %||% guide_none()
    guide_gengrob(guide, theme)
  }
}

guide_for_position <- function(guides, position) {
  if (inherits(guides, "Guides")) {
    pair <- guides$get_position(position)
    return(pair$params)
  }
  has_position <- vapply(
    guides,
    function(guide) identical(guide$position, position),
    logical(1)
  )

  guides <- guides[has_position]
  guides_order <- vapply(guides, function(guide) as.numeric(guide$order)[1], numeric(1))
  Reduce(guide_merge, guides[order(guides_order)])
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

# From ggplot2/R/utilities.r
message_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  message(paste0(wrapped, collapse = "\n"))
}

warning_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  warning(paste0(wrapped, collapse = "\n"), call. = FALSE)
}

is.formula <- function(x) inherits(x, "formula")

# From ggplot2/R/axis-secondary.R
is.sec_axis <- function(x) {
  inherits(x, "AxisSecondary")
}


# From ggplot2/R/theme.r
# renders a theme as complete
#' @importFrom plyr defaults
plot_theme <- function (x, default = theme_get()) {
  theme <- x$theme
  if (is_theme_complete(theme)) {
    theme
  }
  else {
    plyr::defaults(theme, default)
  }
}
is_theme_complete <- function(x) isTRUE(attr(x, "complete"))
