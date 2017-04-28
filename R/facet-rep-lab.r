#' @include ggplot2.r
NULL

#' Repeat axis lines and labels across all facet panels
#'
#' \code{\link[ggplot2]{facet_grid}} and \code{\link[ggplot2]{facet_wrap}}, but
#' with axis lines and labels preserved on all panels.
#'
#' @param ... Arguments used for \code{\link[ggplot2]{facet_grid}} or
#'            \code{\link[ggplot2]{facet_wrap}}.
#' @param repeat.tick.labels Logical, removes the labels (numbering) on an inner
#'                           axes when \code{FALSE}.
#' @rdname facet_rep
#' @export
facet_rep_grid <- function(..., repeat.tick.labels=FALSE) {
  f <- ggplot2::facet_grid(...)
  params <- append(f$params, list(repeat.tick.labels=repeat.tick.labels))
  ggplot2::ggproto(NULL, FacetGridRepeatLabels,
          shrink=f$shrink,
          params=params)
}

#' Removes labels from axis grobs.
#'
#' Called from FacetGridRepeatLabels.
#'
#' @param axisgrob Grob with an axis.
#' @keywords interal
remove_labels_from_axis <- function(axisgrob) {
  if (inherits(axisgrob, 'zeroGrob')) return(axisgrob)
  a <- which(sapply(axisgrob$children, `[[`, i='name') == 'axis')
  d <- grepl('titleGrob', sapply(axisgrob$children[[a]]$grobs, `[[`, i='name'))
  if (sum(d) > 0) {
    axisgrob$children[[a]] <- do.call(gList, axisgrob$children[[a]]$grobs[!d])
    if (length(axisgrob$width$arg1) == 2) axisgrob$width$arg1 <- axisgrob$width$arg1[attr(axisgrob$width$arg1, 'unit') != 'grobwidth']
    if (length(axisgrob$height$arg1) == 2) axisgrob$height$arg1 <- axisgrob$height$arg1[attr(axisgrob$height$arg1, 'unit') != 'grobheight']
    #if (length(axisgrob$children[[a]]$heights) == 2) axisgrob$children[[a]]$heights <- axisgrob$children[[a]]$heights[!d]
  }
  axisgrob
}

#' @rdname splot-ggproto
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

    axis_b <- table$grobs[grepl('axis-b-[[:digit:]]+', table$layout$name)]
    axis_l <- table$grobs[grepl('axis-l-[[:digit:]]+', table$layout$name)]
    axis_t <- table$grobs[grepl('axis-t-[[:digit:]]+', table$layout$name)]
    axis_r <- table$grobs[grepl('axis-r-[[:digit:]]+', table$layout$name)]

    if (params$repeat.tick.labels == FALSE) {
      axis_b <- lapply(axis_b, remove_labels_from_axis)
      axis_l <- lapply(axis_l, remove_labels_from_axis)
      axis_t <- lapply(axis_t, remove_labels_from_axis)
      axis_r <- lapply(axis_r, remove_labels_from_axis)
    }

    panel_range <- find_panel(table)
    panel_range[,c('col','row')] <- c(max(panels$col), max(panels$row))

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

