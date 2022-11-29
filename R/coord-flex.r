# Originally from https://raw.githubusercontent.com/stefanedwards/ggplot2/master/R/brackets.r
# Pushed to ggplot2 with https://github.com/tidyverse/ggplot2/pull/2099

# Flexible Cartesian coordinates ----------------------------------------------
# ggproto objects for these are defined in a later chunk.

#' @include ggplot2.r
NULL

#' Cartesian coordinates with flexible options for drawing axes
#'
#' Allows user to inject a function for drawing axes, such as
#' \code{\link{capped_horizontal}} or \code{\link{brackets_horizontal}}.
#'
#' NB! A panel-border is typically drawn on top such that it covers tick marks,
#' grid lines, and axis lines.
#' Many themes also do not draw axis lines.
#' To ensure the modified axis lines are visible, use
#' \code{theme(panel.border=element_blank(), axis.line=element_line())}.
#'
#' @section User defined functions:
#' The provided function in \code{top}, \code{right}, \code{bottom}, and \code{left}
#' defaults to \code{render_axis} which is defined in \file{ggplot2/R/coord-.r}, which in
#' turns calls \code{guide_axis} (see \file{ggplot2/R/guides-axis.r}).
#'
#' The provided function is with the arguments
#' \code{scale_details}, \code{axis}, \code{scale}, \code{position}, and \code{theme},
#' and the function should return an \code{\link{absoluteGrob}} object.
#'
#' For examples of modifying the drawn object, see e.g.
#' \code{\link{capped_horizontal}} or \code{\link{brackets_horizontal}}.
#'
#' @rdname coord_flex
#' @param top,left,bottom,right Function for drawing axis lines, ticks, and labels,
#'    use e.g. \code{\link{capped_horizontal}} or \code{\link{brackets_horizontal}}.
#  @inheritParams coord_cartesian
#' @param xlim,ylim Limits for the x and y axes.
#' @param expand If \code{TRUE}, the default, adds a small expansion factor to
#'   the limits to ensure that data and axes don't overlap. If \code{FALSE},
#'   limits are taken exactly from the data or \code{xlim}/\code{ylim}.
#' @export
#' @examples
#' library(ggplot2)
#' # A standard plot
#' p <- ggplot(mtcars, aes(disp, wt)) +
#'  geom_point() +
#'  geom_smooth() + theme(panel.border=element_blank(), axis.line=element_line())
#'
#' # We desire that left axis does not extend beyond '6'
#' # and the x-axis is unaffected
#' p + coord_capped_cart(left='top')
#'
#' # Specifying 'bottom' caps the axis with at most the length of 'gap'
#' p + coord_capped_cart(left='top', bottom='none')
#'
#' # We can specify a ridiculus large 'gap', but the lines will always
#' # protrude to the outer most ticks.
#' p + coord_capped_cart(left='top', bottom='none', gap=2)
#'
#' # We can use 'capped_horizontal' and 'capped_vertical' to specify for
#' # each axis individually.
#' p + coord_capped_cart(left='top', bottom=capped_horizontal('none', gap=2))
#'
#' # At this point we might as well drop using the short-hand and go full on:
#' p + coord_flex_cart(left=brackets_vertical(), bottom=capped_horizontal('left'))
#'
#' # Also works with secondary axes:
#' p + scale_y_continuous(sec.axis=sec_axis(~5*., name='wt times 5')) +
#'   coord_flex_cart(left=brackets_vertical(), bottom=capped_horizontal('right'),
#'   right=capped_vertical('both', gap=0.02))
#'
#'
#' # Supports the usual 'coord_fixed':
#' p + coord_flex_fixed(ratio=1.2, bottom=capped_horizontal('right'))
#'
#' # and coord_flip:
#' p + coord_flex_flip(ylim=c(2,5), bottom=capped_horizontal('right'))
coord_flex_cart <- function(xlim = NULL,
                            ylim = NULL,
                            expand = TRUE,
                            top = waiver(),
                            left = waiver(),
                            bottom = waiver(),
                            right = waiver()) {

  test_orientation(top, right, bottom, left)

  ggproto(NULL, CoordFlexCartesian,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    top = top,
    left = left,
    bottom = bottom,
    right = right
  )
}

#' @rdname coord_flex
#' @export
#  @inheritParams  coord_flex_cart
#'
coord_flex_flip <- function(xlim = NULL,
                            ylim = NULL,
                            expand = TRUE,
                            top = waiver(),
                            left = waiver(),
                            bottom = waiver(),
                            right = waiver()) {

  test_orientation(top, right, bottom, left)

  ggproto(NULL, CoordFlexFlipped,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          top = top,
          left = left,
          bottom = bottom,
          right = right
  )
}

#' @rdname coord_flex
#  @inheritParams  coord_flex_cart
#' @export
#' @param ratio aspect ratio, expressed as \code{y / x}.
coord_flex_fixed <- function(ratio = 1,
                             xlim = NULL,
                             ylim = NULL,
                             expand = TRUE,
                             top = waiver(),
                             left = waiver(),
                             bottom = waiver(),
                             right = waiver()) {

  test_orientation(top, right, bottom, left)

  ggproto(NULL, CoordFlexFixed,
          limits = list(x = xlim, y = ylim),
          ratio = ratio,
          expand = expand,
          top = top,
          left = left,
          bottom = bottom,
          right = right
  )
}



# Helper functions ------------------------------------------------------------


# flex_render_axis_h and _v were lifted from the render_axis_h and _v of
# ancestral class "Coord" in coord-.r
# For each top/bottom or left/right axis, they basically just call what ever
# function the coord_flex classes were given.
flex_render_axis_h <- function(self, panel_params, theme) {
  top <- self$top %|W|% panel_guides_grob
  bottom <- self$bottom %|W|% panel_guides_grob
  list(
    top = top(panel_params$guides, position = "top", theme = theme),
    bottom = bottom(panel_params$guides, position = "bottom", theme = theme)
  )
}
flex_render_axis_v <- function(self, panel_params, theme) {
  left <- self$left %|W|% panel_guides_grob
  right <- self$right %|W|% panel_guides_grob
  list(
    left = left(panel_params$guides, position = "left", theme = theme),
    right = right(panel_params$guides, position = "right", theme = theme)
  )
}

# Checks that the provided axis function corresponds to the orientation of the
# axis it is used upon.
test_orientation <- function(top, right, bottom, left) {
  if (!is.waive(top) &&
      !is.null(attr(top, 'orientation', exact=TRUE)) &&
      attr(top, 'orientation', exact=TRUE) == 'vertical')
    stop('`top` has been supplied a vertical axis function; this will not work.')
  if (!is.waive(bottom) &&
      !is.null(attr(bottom, 'orientation', exact=TRUE)) &&
      attr(bottom, 'orientation', exact=TRUE) == 'vertical')
    stop('`bottom` has been supplied a vertical axis function; this will not work.')
  if (!is.waive(left) &&
      !is.null(attr(left, 'orientation', exact=TRUE)) &&
      attr(left, 'orientation', exact=TRUE) == 'horizontal')
    stop('`left` has been supplied a horizontal axis function; this will not work.')
  if (!is.waive(right) &&
      !is.null(attr(right, 'orientation', exact=TRUE)) &&
      attr(right, 'orientation', exact=TRUE) == 'horizontal')
    stop('`right` has been supplied a horizontal axis function; this will not work.')
}


# ggproto objects -------------------------------------------------------------

#' @rdname lemon-ggproto
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
CoordFlexCartesian <- ggplot2::ggproto('CoordFlexCartesian',
    `_inherit` = ggplot2::CoordCartesian,
    render_axis_h = flex_render_axis_h,
    render_axis_v = flex_render_axis_v
)

#' @rdname lemon-ggproto
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
CoordFlexFlipped <- ggplot2::ggproto('CoordFlexFlipped',  `_inherit` = ggplot2::CoordFlip,
                            render_axis_h = flex_render_axis_h,
                            render_axis_v = flex_render_axis_v
)

#' @rdname lemon-ggproto
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
CoordFlexFixed <- ggplot2::ggproto('CoordFlexFlipped',  `_inherit` = ggplot2::CoordFixed,
                          render_axis_h = flex_render_axis_h,
                          render_axis_v = flex_render_axis_v
)
