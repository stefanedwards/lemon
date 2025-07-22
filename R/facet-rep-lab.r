#' @include ggplot2.r
NULL

#' Repeat axis lines and labels across all facet panels
#'
#' \code{\link[ggplot2]{facet_grid}} and \code{\link[ggplot2]{facet_wrap}}, but
#' with axis lines and labels preserved on all panels.
#'
#' These two functions are extensions to \code{\link[ggplot2]{facet_grid}}
#' and \code{\link[ggplot2]{facet_wrap}} that keeps axis lines, ticks, and
#' optionally tick labels across all panels.
#'
#' Examples are given in the vignette \href{../doc/facet-rep-labels.html}{"Repeat axis lines on facet panels" vignette}.
#'
#' @param ... Arguments used for \code{\link[ggplot2]{facet_grid}} or
#'            \code{\link[ggplot2]{facet_wrap}}.
#' @param repeat.tick.labels When \code{FALSE} (default), axes on inner panels
#'                           have their tick labels (i.e. the numbers) removed.
#'                           Set this to \code{TRUE} to keep all labels,
#'                           or any combination of top, bottom, left, right to
#'                           keep only those specified. Also acceps 'x' and 'y'.
#' @rdname facet_rep
#' @export
facet_rep_grid <- function(..., repeat.tick.labels=FALSE) {
  f <- ggplot2::facet_grid(...)

  rtl <- reduce.ticks.labels.settings(repeat.tick.labels)
  # if (scales %in% c('free','free_y') && !any(c('left','right') %in% rtl))
  #   rtl <- c(rtl, 'left')
  # if (scales %in% c('free','free_x') && !any(c('top','bottom') %in% rtl))
  #   rtl <- c(rtl, 'bottom')

  params <- append(f$params, list(repeat.tick.labels=rtl))
  ggplot2::ggproto(NULL, FacetGridRepeatLabels,
          shrink=f$shrink,
          params=params)
}

#' Reduces the multitude of repeat.tick.labels-settings to
#' a a combination of four sides.
#' Empty vector corresponds to 'none'
#'
#' @noRd
reduce.ticks.labels.settings <- function(ticks) {
  if (length(ticks) == 0) return(character(0))
  if (length(ticks) == 1 && ticks == FALSE) return(character(0))
  if (length(ticks) == 1 && ticks == TRUE) return(c('top', 'right','bottom','left') )
  ticks <- tolower(ticks)
  ticks <- match.arg(tolower(ticks), c('none','all','x','y','left','top','bottom','right'), several.ok=TRUE)
  if ('none' %in% ticks) return(character(0))
  if ('all' %in% ticks) return(c('top', 'right','bottom','left'))
  if ('x' %in% ticks) ticks <- c(ticks[ticks!='x'], 'top','bottom')
  if ('y' %in% ticks) ticks <- c(ticks[ticks!='y'], 'right', 'left')
  return(ticks)
}

#' Removes labels from axis grobs.
#'
#' Called from FacetGridRepeatLabels.
#'
#' @param axisgrob Grob with an axis.
#' @param direction Whether the axis is horizontal or vertical.
#' @keywords interal
remove_labels_from_axis <- function(axisgrob, direction = c('horizontal','vertical')) {
  direction <- match.arg(direction)
  if (inherits(axisgrob, 'zeroGrob')) return(axisgrob)

  a <- which(sapply(axisgrob$children, `[[`, i='name') == 'axis')
  d <- which(grepl('titleGrob', sapply(axisgrob$children[[a]]$grobs, `[[`, i='name')))

  if (length(d) == 1) {
    old_axisgrob <- axisgrob
    axis <- axisgrob$children[[a]]
    axis$grobs[[d]] <- zeroGrob()
    if (direction == 'horizontal') {
      i <- unique(axis$layout$t[d])
      axis$heights[i] <- unit(0,'cm')
      axisgrob$height <- sum(axis$heights)
    } else if (direction == 'vertical') {
      i <- unique(axis$layout$l[d])
      axis$widths[i] <- unit(0,'cm')
      axisgrob$width <- sum(axis$widths)
    }
    axisgrob$children[[a]] <- axis
    axisgrob$vp <- NULL ## ugly hack, I don't like it.
  }
  axisgrob
}

#' @section Facets:
#' The class `FacetGridRepeatLabels` is an extension of [ggplot2::FacetGrid]
#'  while `FacetWrapRepeatLabels` is an extension of [ggplot2::FacetWrap],
#' both draws the axes on each panel.
#' See [facet_rep_grid], [facet_rep_wrap], `facet-rap-lab.r` and `facet_wrap.r`.
#'
#' @md
#' @rdname lemon-ggproto
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import gtable
FacetGridRepeatLabels <- ggplot2::ggproto('FacetGridRepeatLabels',
                                          `_inherit`=ggplot2::FacetGrid,
  draw_panels  = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    table <- ggplot2::FacetGrid$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)

    # Add axes across all panels
    panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
    panels$col <- as.integer(as.factor(panels$l))
    panels$row <- as.integer(as.factor(panels$t))

    panel_range <- find_panel(table)
    panel_range$col <- max(panels$col)
    panel_range$row <- max(panels$row)

    axis_b <- table$grobs[grepl('axis-b-[[:digit:]]+', table$layout$name)]
    axis_l <- table$grobs[grepl('axis-l-[[:digit:]]+', table$layout$name)]
    axis_t <- table$grobs[grepl('axis-t-[[:digit:]]+', table$layout$name)]
    axis_r <- table$grobs[grepl('axis-r-[[:digit:]]+', table$layout$name)]

    if (!'bottom' %in% params$repeat.tick.labels && panel_range$row > 1)
      axis_b <- lapply(axis_b, remove_labels_from_axis, direction='horizontal')
    if (!'left' %in% params$repeat.tick.labels && panel_range$col > 1)
      axis_l <- lapply(axis_l, remove_labels_from_axis, direction='vertical')
    if (!'top' %in% params$repeat.tick.labels && panel_range$col > 1)
      axis_t <- lapply(axis_t, remove_labels_from_axis, direction='horizontal')
    if (!'right' %in% params$repeat.tick.labels && panel_range$row > 1)
      axis_r <- lapply(axis_r, remove_labels_from_axis, direction='vertical')




    l_axis_column_added <- logical(panel_range$col)
    r_axis_column_added <- logical(panel_range$col)
    t_axis_row_added <- logical(panel_range$row)
    b_axis_row_added <- logical(panel_range$row)

    for (i in nrow(panels):1) {
      p <- panels[i,]
      # Bottom
      if (p$row != panel_range$row & !inherits(axis_b[[p$col]], 'zeroGrob')) { # panel not in bottom row, (add row), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (b_axis_row_added[p$row] == FALSE) {
          b_axis_row_added[p$row] <- TRUE
          table <- gtable::gtable_add_rows(table, max_height(axis_b), coord$b)
        }
        table <- gtable::gtable_add_grob(table, axis_b[[p$col]], t=coord$b+1, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-b-%d-%d', p$col, p$row))
      }
      # Left
      if (p$col > 1 & !inherits(axis_l[[p$row]], 'zeroGrob')) { # panel is not left-most column, (add column), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (l_axis_column_added[p$col] == FALSE) {
          l_axis_column_added[p$col] <- TRUE
          table <- gtable::gtable_add_cols(table, max_width(axis_l), coord$l-1)
          table <- gtable::gtable_add_grob(table, axis_l[[p$row]], t=coord$t, b=coord$b, l=coord$l, clip='off', name=sprintf('axis-l-%d-%d', p$row, p$col))
        } else {
          table <- gtable::gtable_add_grob(table, axis_l[[p$row]], t=coord$t, b=coord$b, l=coord$l-1, clip='off', name=sprintf('axis-l-%d-%d', p$row, p$col))
        }
      }
      # Top
      if (p$row > 1 & !inherits(axis_t[[p$col]], 'zeroGrob')) { # panel not in top row, (add row), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (t_axis_row_added[p$row] == FALSE) {
          t_axis_row_added[p$row] <- TRUE
          table <- gtable::gtable_add_rows(table, max_height(axis_t), coord$t-1)
          table <- gtable::gtable_add_grob(table, axis_t[[p$col]], t=coord$t, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-t-%d-%d', p$col, p$row))
        } else {
          table <- gtable::gtable_add_grob(table, axis_t[[p$col]], t=coord$t-1, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-t-%d-%d', p$col, p$row))
        }

      }
      # Right
      if (p$col != panel_range$col & !inherits(axis_l[[p$row]], 'zeroGrob')) { # panel is not right-most, (add colun), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (r_axis_column_added[p$col] == FALSE) {
          r_axis_column_added[p$col] <- TRUE
          table <- gtable::gtable_add_cols(table, max_width(axis_r), coord$r)
        }
        table <- gtable::gtable_add_grob(table, axis_r[[p$row]], t=coord$t, b=coord$b, l=coord$r+1, clip='off', name=sprintf('axis-r-%d-%d', p$row, p$col))
      }

    }

    table
  }
)

