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
#' @export
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# From ggplot2/R/coord-.r
# Renders an axis with the correct orientation or zeroGrob if no axis should be
# generated
# render_axis <- function(panel_params, axis, scale, position, theme) {
#   browser()
#   if (axis == "primary") {
#     guide_axis(panel_params[[paste0(scale, ".major")]], panel_params[[paste0(scale, ".labels")]], position, theme)
#   } else if (axis == "secondary" && !is.null(panel_params[[paste0(scale, ".sec.major")]])) {
#     guide_axis(panel_params[[paste0(scale, ".sec.major")]], panel_params[[paste0(scale, ".sec.labels")]], position, theme)
#   } else {
#     zeroGrob()
#   }
# }


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
  guide <- guide_for_position(guides, position) %||% guide_none(title = waiver())
  guide$title %|W|% default_label
}

panel_guides_grob <- function(guides, position, theme) {
  guide <- guide_for_position(guides, position) %||% guide_none()
  guide_gengrob(guide, theme)
}

guide_for_position <- function(guides, position) {
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

# From ggplot2/R/theme.r
#' Combine the properties of two elements
#'
#' @param e1 An element object
#' @param e2 An element object from which e1 inherits
#'
#' @noRd
#'
combine_elements <- function(e1, e2) {

  # If e2 is NULL, nothing to inherit
  if (is.null(e2) || inherits(e1, "element_blank")) {
    return(e1)
  }

  # If e1 is NULL inherit everything from e2
  if (is.null(e1)) {
    return(e2)
  }

  # If neither of e1 or e2 are element_* objects, return e1
  if (!inherits(e1, "element") && !inherits(e2, "element")) {
    return(e1)
  }

  # If e2 is element_blank, and e1 inherits blank inherit everything from e2,
  # otherwise ignore e2
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) {
      return(e2)
    } else {
      return(e1)
    }
  }

  # If e1 has any NULL properties, inherit them from e2
  n <- names(e1)[vapply(e1, is.null, logical(1))]
  e1[n] <- e2[n]

  # Calculate relative sizes
  if (is.rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }

  # Calculate relative linewidth
  if (is.rel(e1$linewidth)) {
    e1$linewidth <- e2$linewidth * unclass(e1$linewidth)
  }

  e1
}

is.rel <- function(x) { inherits(x, 'rel') }


# from ggplot2/R/theme-elements.r
# Returns NULL if x is length 0
len0_null <- function(x) {
  if (length(x) == 0)  NULL
  else                 x
}

