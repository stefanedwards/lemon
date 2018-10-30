# Files given here are from ggplot2 base.
#' @import ggplot2
#' @import grid
#' @import gtable
NULL

# I should probably
#' @include ggplot2.r

#' Given a theme object and element name, return a grob for the element
#' 
#' @rdname element_render
#' @author \code{element_render} is from ggplot2 source.
#' @export
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

#' Render a ggplot2 grob or retrieve its gpar object.
#' 
#' Helps add the ggplot2-theme's look-and-feel to \code{grid}'s grob objects.
#' \code{render_gpar} returns a \code{\link[grid]{gpar}}-object,
#' \code{element_render} returns a \code{\link[grid]{grid.grob}}-object.
#' 
#' @param theme A ggplot2 \link[ggplot2]{theme}
#' @param element The name of an element in the theme, e.g. "axis.text".
#' @param ... Additional arguments sent to grobs (e.g. \code{x} or \code{y}).
#' @param name Returned grob's name..
#' @return A \code{\link[grid]{grid.grob}} or \code{\link[grid]{gpar}} object.
#' @seealso \code{\link[ggplot2]{theme}}
#' @rdname element_render
#' @export
render_gpar <- function(theme, element, ...) {
  gp <- element_render(theme, element)
  gp$gp %||% gp$children[[1]]$gp
}

#' Version safe(r) method to get the y- and x-range from trained scales.
#' 
#' The names of the internal layout objects from \code{ggplot_build} changed
#' slightly.
#' @name get_panel_range
#' @param layout \code{layout} part from ggplot_build
#' @param index Could be panel number?
#' @export
get_panel_y_range <- function(layout, index=1) {
  layout$panel_params[[index]]$y.range %||% layout$panel_ranges[[index]]$y.range
}

#' @rdname get_panel_range
#' @inheritParams get_panel_y_range
#' @export
get_panel_x_range <- function(layout, index=1) {
  layout$panel_params[[index]]$x.range %||% layout$panel_ranges[[index]]$x.range
}

#' @rdname get_panel_range
#' @inheritParams get_panel_y_range
#' @export
get_panel_params <- function(layout, index=1) {
  layout$panel_params[[index]] %||% layout$panel_ranges[[index]]
}

