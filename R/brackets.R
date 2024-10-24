# Originally from https://raw.githubusercontent.com/stefanedwards/ggplot2/master/R/brackets.r
# Pushed to ggplot2 with https://github.com/tidyverse/ggplot2/pull/2099

#' @include ggplot2.r
NULL

#' Axis brackets instead of axis ticks and lines
#'
#' To be used with \code{\link{coord_flex_cart}},
#' \code{\link{coord_capped_cart}}, etc. for displaying brackets instead
#' of the axis ticks and lines.
#'
#' The looks of the brackets are taken from \code{theme(axis.ticks)}, or
#' \code{theme(axis.ticks.x)} and \code{theme(axis.ticks.y)}, respectively.
#'
#' It does not re-calculate tick marks, but lets \code{scale_x_*} and \code{scale_y_*}
#' calculate and draw ticks and labels, and then modifies the ticks with brackets.
#'
#' Both \code{length} and \code{tick.length} accepts a numeric scalar instead of
#' a \code{\link[grid]{unit}} object that is interpreted as an \code{"npc"} unit.
#'
#' @export
#' @rdname brackets
#' @param direction Which way should the opening side of the brackets point?
#'   up, down, left, or right?
#' @param length Length of the unit, parallel with axis line.
#' @param tick.length Height (width) of x-axis (y-axis) bracket.
#'   If \code{waiver()} (default), use \code{axis.ticks.length} from \code{\link[ggplot2]{theme}}.
#' @seealso \code{\link[grid]{unit}}
#' @aliases brackets_horisontal
#' @examples
#' library(ggplot2)
#' p <- ggplot(mpg, aes(as.factor(cyl), hwy, colour=class)) +
#'   geom_point(position=position_jitter(width=0.3)) +
#'   theme_bw() +
#'   theme(panel.border = element_blank(), axis.line = element_line())
#' p
#'
#' p <- p + coord_flex_cart(bottom=brackets_horizontal(length=unit(0.08, 'npc')))
#' p
#' # However getting the correct width is a matter of tweaking either length or
#' # position_jitter...
#'
#' # A further adjustment,
#' p + theme(panel.grid.major.x = element_blank())
#' @import grid
#' @import ggplot2
#' @import gtable
brackets_horizontal <- function(direction = c('up','down'),
                                length = unit(0.05, 'npc'),
                                tick.length = waiver()) {

  if (!grid::is.unit(length))
    length <- unit(as.numeric(length), 'npc')

  if (!is.waive(tick.length) && !grid::is.unit(tick.length))
    tick.length <- unit(as.numeric(tick.length), 'npc')

  direction=match.arg(direction)

  # Returns a function
  fn <- function(guides, position, theme) {
    agrob <- panel_guides_grob(guides, position, theme)
    if (agrob$name == 'NULL') return(agrob)

    ind <- names(agrob$children) == 'axis'
    ind.notline <-  which(ind)
    ind.ticks <- which(grepl('polyline', sapply(agrob$children[[ind.notline]]$grobs, `[[`, i = 'name')))
    ind.text <- which(grepl('title', sapply(agrob$children[[ind.notline]]$grobs, `[[`, i = 'name')))
    ticksgrob <- agrob$children[[ind.notline]]$grobs[[ind.ticks]]

    # If theme(axis.ticks[.x/.y] = element_blank()), then ticksgrob is a
    # zeroGrob and we cannot change the ticks (polylineGrob), as $gp is NULL.
    if (!is.zero(ticksgrob) && !is.null(ticksgrob$gp)) {
      gp <- do.call(grid::gpar, ticksgrob$gp)
      nticks <- length(ticksgrob$id.lengths)

      x <- rep(ticksgrob$x, each = 2) +
        rep(unit.c(length * -1, length * -1, length, length ), times = nticks)
      tick.length <- tick.length %|W|% theme$axis.ticks.length
      y0 <-  unit.c(unit(1, 'npc'), unit(1, 'npc') - tick.length)
      d <- switch(direction, down = c(2, 1, 1, 2), up=c(1, 2, 2, 1))
      y <- rep(y0[d], times = nticks)
      id.lengths <- rep(4, times = nticks)
      brackets <- polylineGrob(x = x, y = y, id.lengths = id.lengths, gp = gp)
    } else {
      tick.length <- unit(0, 'npc')
      brackets <- zeroGrob()
    }

    if (length(ind.text)) {
      labels <- agrob$children[[ind.notline]]$grobs[[ind.text]]
    } else {
      labels <- zeroGrob()
    }


    gt <- switch(position,
      top = gtable::gtable_col('axis',
        grobs = list(labels, brackets),
        width = unit(1, 'npc'),
        heights = unit.c(grobHeight(labels), tick.length)
      ),
      bottom = gtable::gtable_col('axis',
        grobs = list(brackets, labels),
        width = unit(1, 'npc'),
        heights = unit.c(tick.length, grobHeight(labels))
      )
    )
    justvp <- switch(position,
      top = viewport(y = 0, just ='bottom', height = gtable_height(gt)),
      bottom = viewport(y = 1, just = 'top', height = gtable_height(gt))
    )

    absoluteGrob(
      gList(gt),
      width = gtable::gtable_width(gt),
      height = gtable::gtable_height(gt),
      vp = justvp
    )
  }
  attr(fn, 'orientation') <- 'horizontal'
  fn
}

## Turns out, there is no British spelling horizontal with an s...
#' @export
#' @keywords internal
#' @inheritParams brackets_horizontal
brackets_horisontal <- brackets_horizontal

#' @export
#' @rdname brackets
#  @inheritParams brackets_horizontal
brackets_vertical <- function(direction = c('left','right'),
                              length = unit(0.05, 'npc'),
                              tick.length = waiver()) {

  if (!grid::is.unit(length))
    length <- unit(as.numeric(length), 'npc')

  if (!is.waive(tick.length) && !grid::is.unit(tick.length))
    tick.length <- unit(as.numeric(tick.length), 'npc')

  direction=match.arg(direction)
  fn <- function(guides, position, theme) {
    agrob <- panel_guides_grob(guides, position, theme)
    if (agrob$name == 'NULL') return(agrob)

    ind <- names(agrob$children) == 'axis'
    ind.notline <-  which(ind)
    ind.ticks <- which(grepl('polyline', sapply(agrob$children[[ind.notline]]$grobs, `[[`, i = 'name')))
    ind.text <- which(grepl('title', sapply(agrob$children[[ind.notline]]$grobs, `[[`, i = 'name')))
    ticksgrob <- agrob$children[[ind.notline]]$grobs[[ind.ticks]]


    # If theme(axis.ticks[.x/.y] = element_blank()), then ticksgrob is a
    # zeroGrob and we cannot change the ticks (polylineGrob), as $gp is NULL.
    if (!is.zero(ticksgrob) && !is.null(ticksgrob$gp)) {
      gp <- do.call(grid::gpar, ticksgrob$gp)
      nticks <- length(ticksgrob$id.lengths)

      y <- rep(ticksgrob$y, each = 2) +
        rep(unit.c(length * -1, length * -1, length, length ), times = nticks)
      tick.length <- tick.length %|W|% theme$axis.ticks.length
      x0 <-  unit.c(unit(1, 'npc'), unit(1, 'npc') - tick.length)
      d <- switch(direction, left = c(2, 1, 1, 2), right=c(1, 2, 2, 1))
      x <- rep(x0[d], times = nticks)
      id.lengths <- rep(4, times = nticks)
      brackets <- polylineGrob(x = x, y = y, id.lengths = id.lengths, gp = gp)
      labels <- agrob$children[[ind.notline]]$grobs[[ind.text]]
    } else {
      labels <- agrob$children[[ind.notline]]$grobs[[ind.text]]
      tick.length <- unit(0, 'npc')
      brackets <- zeroGrob()
    }

    gt <- switch(position,
      left = gtable::gtable_row('axis',
        grobs = list(labels, brackets),
        height = unit(1, 'npc'),
        widths = unit.c(grobWidth(labels), tick.length)
      ),
      right = gtable::gtable_row('axis',
        grobs = list(brackets, labels),
        height = unit(1, 'npc'),
        widths = unit.c(tick.length, grobWidth(labels))
      )
    )
    justvp <- switch(position,
      left = viewport(x = 1, just = 'right', width = gtable_width(gt)),
      right = viewport(x = 0, just = 'left', width = gtable_width(gt))
    )

    absoluteGrob(
      gList(gt),
      width = gtable::gtable_width(gt),
      height = gtable::gtable_height(gt),
      vp = justvp
    )
  }
  attr(fn, 'orientation') <- 'vertical'
  fn
}
