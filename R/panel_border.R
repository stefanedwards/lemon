#' Box theme element
#'
#' Drop-in replacement for `element_rect`, where the four sides
#' can be specified individually.
#'
#' Useful for specifying panels' borders, e.g. left-bottom lines, in facets
#' without modifying the axis-lines/-ticks.
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang is_missing maybe_missing
#' @param sides Any combination of `"top"`,`"left"`,`"right"`,`"bottom"`,
#'   `"horizontal"`,`"vertical"`,`"L"`,`"all"`,`"none"`,`TRUE`,`FALSE`.
#' @param left,top,right,bottom A theme element (`element_line`); overruled by `sides`.
#' @param fill If set, causes a box to be drawn filled with this color.
#' @param inherit.blank Same as for `element_rect` and `element_line`.
#' @param ... Additional parameters used for `element_line` for `sides`.
#' @export
#' @md
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + theme(panel.border = element_box("L", color="blue"))
element_box <- function(sides, left, top, right, bottom, fill=NULL, inherit.blank = FALSE, ...) {
  if (is_missing(sides)) {
    sides <- character(0)
  } else if (length(sides) == 1 && sides == 'L') {
    sides <- c('bottom','left')
  }
  sides <- reduce.ticks.labels.settings(sides)


  default.line <- do.call(element_line, list(inherit.blank = inherit.blank, ...))
  left <- combine_side(maybe_missing(left), default.line, 'left' %in% sides)
  top <- combine_side(maybe_missing(top), default.line, 'top' %in% sides)
  right <- combine_side(maybe_missing(right), default.line, 'right' %in% sides)
  bottom <- combine_side(maybe_missing(bottom), default.line, 'bottom' %in% sides)

  structure(list(fill=fill, left=left, top=top, right=right, bottom=bottom,
    inherit.blank = inherit.blank),
    class = c("element_box","element_rect","element"))
}

combine_side <- function(side, default, use_default_on_side) {
  if (!use_default_on_side && !is_missing(side)) return(side)
  if (!use_default_on_side && is_missing(side)) return(zeroGrob())
  if (is_missing(side)) return(default)
  combine_elements(default, side)
}

## see ggplot2:::element_grob.element_rect

#' @import grid
#' @exportS3Method ggplot2::element_grob element_box
element_grob.element_box <- function(element,
  x = 0.5, y = 0.5, width = 1, height = 1, fill = NULL,
  colour = NULL, linewidth = NULL, linetype = NULL, ...) {

  back <- if (is.null(fill)) zeroGrob() else
    rectGrob(x, y, width, height, gp = gpar(fill = fill))

  left <- element_grob(element$left, x = unit(c(0,0), 'npc'), y = unit(c(0,1), 'npc'))
  top <- element_grob(element$top, x = unit(c(0,1), 'npc'), y = unit(c(1,1), 'npc'))
  right <- element_grob(element$right, x = unit(c(1,1), 'npc'), y = unit(c(0,1), 'npc'))
  bottom <- element_grob(element$bottom, x = unit(c(0,1), 'npc'), y = unit(c(0,0), 'npc'))

  grobTree(
    back, left, top, right, bottom,
    name = 'panel_border'
  )
}

#' @import grid
#' @exportS3Method ggplot2::element_grob zeroGrob
element_grob.zeroGrob <- function(element, ...) {
  return(element)
}
